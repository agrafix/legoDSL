{-# LANGUAGE OverloadedStrings #-}
module NXT.Parser.RXEParser where

import qualified Data.ByteString as B
import Data.Binary.Strict.BitGet
import Data.Word
import Data.Bits
import Data.Maybe
import Control.Monad
import Numeric (showHex)

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

data RXE
   = RXE
   { r_header :: Header
   , r_dataspace :: DataSpace
--   , r_clump :: ClumpRecords
--   , r_code :: CodeSpace
   } deriving (Show, Eq)

data Header
   = Header
   { h_version :: Int
   , h_dataspaceHeader :: DataSpaceHeader
   , h_clumps :: Word16
   , h_codeWords :: Word16
   } deriving (Show, Eq)

data DataSpaceHeader
   = DataSpaceHeader
   { ds_count :: Word16
   , ds_init_size :: Word16
   , ds_static_size :: Word16
   , ds_default_data_size :: Word16
   , ds_dyn_default_offset :: Word16
   , ds_dyn_default_size :: Word16
   , ds_mem_manager_head :: Word16
   , ds_mem_manager_tail :: Word16
   , ds_dope_vector_offset :: Word16
   } deriving (Show, Eq)

data RecordType
   = TC_VOID
   | TC_UBYTE
   | TC_SBYTE
   | TC_UWORD
   | TC_SWORD
   | TC_ULONG
   | TC_SLONG
   | TC_ARRAY
   | TC_CLUSTER
   | TC_MUTEX
   | TC_FLOAT
   deriving (Show, Eq, Enum)

data DefaultValue
   = PlaceholderV
   | ByteV Word8
   | WordV Word16
   | LongV Word32
   | ArrayV
   | ClusterV
   | MutexV -- Word32
   | FloatV Word32
   deriving (Show, Eq)

defaultValueSize (ByteV _) = 1
defaultValueSize (WordV _) = 2
defaultValueSize (LongV _) = 4
defaultValueSize (FloatV _) = 4
defaultValueSize _ = 0

data DopeVector
   = DopeVector
   { dv_offset :: Word16
   , dv_elSize :: Word16
   , dv_elCount :: Word16
   , dv_backPointer :: Word16
   , dv_linkIdx :: Word16
   } deriving (Show, Eq)

data DataSpace
   = DataSpace
   { ds_dstoc :: [(RecordType, Word8, Word16)]
   , ds_default_static :: [DefaultValue]
   , ds_default_dyn :: [DefaultValue]
   , ds_dope_vecs :: [DopeVector]
   } deriving (Show, Eq)

bytes i = i*8

parseDataSpaceH :: BitGet DataSpaceHeader
parseDataSpaceH =
    do ct <- getWord16le
       isize <- getWord16le
       statSize <- getWord16le
       defData <- getWord16le
       dynOff <- getWord16le
       dynSize <- getWord16le
       memHead <- getWord16le
       memTail <- getWord16le
       vec <- getWord16le
       return $ DataSpaceHeader ct isize statSize defData dynOff dynSize memHead memTail vec

parseHeader :: BitGet Header
parseHeader =
    do -- format string
       nxt <- getLeftByteString $ bytes 13
       when (nxt /= "MindstormsNXT") $ fail ("Wrong input file (" ++ (show nxt) ++ ")")
       skip $ bytes 1
       vers <- getAsWord16 $ bytes 2
       -- dataspace header
       ds <- parseDataSpaceH
       -- clump count
       clumps <- getWord16le
       -- word count
       words <- getWord16le
       return $ Header (fromIntegral vers) ds clumps words

getRecordType :: Word8 -> RecordType
getRecordType 0 = TC_VOID
getRecordType 1 = TC_UBYTE
getRecordType 2 = TC_SBYTE
getRecordType 3 = TC_UWORD
getRecordType 4 = TC_SWORD
getRecordType 5 = TC_ULONG
getRecordType 6 = TC_SLONG
getRecordType 7 = TC_ARRAY
getRecordType 8 = TC_CLUSTER
getRecordType 9 = TC_MUTEX
getRecordType 10 = TC_FLOAT

parseDSTOC :: BitGet (RecordType, Word8, Word16)
parseDSTOC =
    do ty <- getWord8
       flag <- getWord8
       desc <- getWord16le
       return (getRecordType ty, flag, desc)

parseStaticDef :: (RecordType, Word8, Word16) -> BitGet (Maybe DefaultValue)
parseStaticDef (_, 1, _) = return $ Nothing
parseStaticDef (TC_VOID, _, _) = return $ Just PlaceholderV
parseStaticDef (TC_ARRAY, _, _) = return $ Just ArrayV
parseStaticDef (TC_CLUSTER, _, _) = return $ Just ClusterV
parseStaticDef (TC_MUTEX, _, _) =
    do skip $ bytes 4
       return $ Just MutexV
parseStaticDef (TC_UBYTE, _, _) =
    do w <- getWord8
       return $ Just $ ByteV w
parseStaticDef (TC_SBYTE, _, _) =
    do w <- getWord8
       return $ Just $ ByteV w
parseStaticDef (TC_UWORD, _, _) =
    do w <- getWord16le
       return $ Just $ WordV w
parseStaticDef (TC_SWORD, _, _) =
    do w <- getWord16le
       return $ Just $ WordV w
parseStaticDef (TC_ULONG, _, _) =
    do w <- getWord32le
       return $ Just $ LongV w
parseStaticDef (TC_SLONG, _, _) =
    do w <- getWord32le
       return $ Just $ LongV w
parseStaticDef (TC_FLOAT, _, _) =
    do w <- getWord32le
       return $ Just $ FloatV w

parseDopeVector :: BitGet DopeVector
parseDopeVector =
    do offset <- getWord16le
       elSize <- getWord16le
       elCount <- getWord16le
       backPtr <- getWord16le
       linkIdx <- getWord16le
       return $ DopeVector offset elSize elCount backPtr linkIdx

parseDVs :: DataSpaceHeader -> BitGet [DopeVector]
parseDVs h =
    do skip $ bytes $ (fromIntegral $ ds_dyn_default_offset h) +  ((fromIntegral $ ds_dope_vector_offset h) - (fromIntegral $ ds_static_size h))
       rootDV <- parseDopeVector
       allVecs <- replicateM (fromIntegral $ dv_elCount rootDV) parseDopeVector
       return (rootDV : allVecs)

parseDataSpace :: DataSpaceHeader -> BitGet DataSpace
parseDataSpace h =
    do -- table of contents
       dstocs <- replicateM (fromIntegral $ ds_count h) parseDSTOC
       -- dope vectors
       dVecs <- lookAhead (parseDVs h)
       -- static default vals
       mStaticDef <- mapM parseStaticDef dstocs
       let staticDef = catMaybes mStaticDef
           dSize = sum $ map defaultValueSize staticDef
       when (dSize > (fromIntegral $ ds_dyn_default_offset h)) $
            fail $ "ERROR. Got: " ++ (show dSize) ++ " but header says:" ++ (show $ ds_dyn_default_offset h)
       -- fixme: dynamic default vals
       skip $ bytes $ fromIntegral $ ds_dyn_default_size h

       return $ DataSpace dstocs staticDef [] dVecs

parseRXE :: BitGet RXE
parseRXE =
    do header <- parseHeader
       ds <- parseDataSpace $ h_dataspaceHeader header
       return $ RXE header ds

parse :: FilePath -> IO ()
parse fp =
    do input <- B.readFile fp
       print $ runBitGet input parseRXE

test =
    parse "/Users/athiemann/devel/NXTDSL/test.rxe"
