{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.Foldable
import Data.List
import Data.Word
import System.Environment
import Text.Printf
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as BS

data FramedMessage = FramedMessage
	{ frame :: Word
	, message :: GameMessage
	} deriving (Eq, Ord, Read, Show)

data GameMessage
	= Boring BoringMessage
	| LevelSelect1 Music Word Speed
	| LevelSelect2 Music Word Speed Word Speed
	| Play1 PlayerStatus
	| Play2 PlayerStatus PlayerStatus
	deriving (Eq, Ord, Read, Show)

data Music = Fever | Chill | Off deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Speed = Low | Med | Hi deriving (Bounded, Enum, Eq, Ord, Read, Show)
data BoringMessage = StartScreen | Demo | Boot | Cutscene | Pause | Transition | Glitch
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

data PlayerStatus = PlayerStatus
	{ level :: Word
	, viruses :: Word
	, speed :: Speed
	, extra :: Word
	} deriving (Eq, Ord, Read, Show)

class Parseable a where parser :: Parser a

instance Parseable Word where
	parser = foldl' (\n digit -> 10*n + fromIntegral digit) 0
		<$> many (A.satisfyWith (subtract 48) (\x -> 0 <= x && x <= 9))

string8 :: String -> Parser ByteString
string8 = A.string . BS.pack

enumParser :: (Show a, Bounded a, Enum a) => Parser a
enumParser = asum . map parserFromVal $ [minBound .. maxBound] where
	parserFromVal v = v <$ (string8 . skewerCase . show) v
	skewerCase = drop 1 . concatMap skewerCaseChar
	skewerCaseChar c | isUpper c = [' ', toLower c]
		             | otherwise = [c]

instance Parseable Music where parser = enumParser
instance Parseable Speed where parser = enumParser
instance Parseable BoringMessage where parser = enumParser

space :: Parser Word8
space = A.word8 32

instance Parseable PlayerStatus where
	parser = pure PlayerStatus
		<*> parser
		<*  string8 "("
		<*> parser
		<*  string8 ") "
		<*> parser
		<*  space
		<*> parser

parseLevelSelect :: Parser GameMessage
parseLevelSelect = pure (\m l s f -> f m l s)
	<*  string8 "level select "
	<*> parser
	<*  space
	<*> parser
	<*  space
	<*> parser
	<*> asum
		[ pure (\l2 s2 m l1 s1 -> LevelSelect2 m l1 s1 l2 s2)
		  	<* space <*> parser
		  	<* space <*> parser
		, pure LevelSelect1
		]

parsePlay :: Parser GameMessage
parsePlay = pure (\ps f -> f ps)
	<*  string8 "play "
	<*> parser
	<*> asum
		[ pure (\w1 ps2 ps1 -> Play2 ps1 { extra = w1 } ps2)
		  	<* space <*> parser
		  	<* space <*> parser
		, pure Play1
		]

instance Parseable GameMessage where
	parser = asum [parsePlay, parseLevelSelect, Boring <$> parser]

instance Parseable FramedMessage where
	parser = pure FramedMessage
		<*> parser
		<*  space
		<*> parser

data State
	= Init
	| Ready
	| WrongVirusCount Word
	| Filling Word
	| Playing Word
	| ToppedOut
	| Restarting Word
	deriving (Eq, Ord, Read, Show)

data Mode = VirusCount | Score deriving (Bounded, Enum, Eq, Ord, Read, Show)

virusesFor :: Word -> Word
virusesFor n = 4 * (min n 20 + 1)

transitionVirusCount :: FramedMessage -> State -> (State, ByteString)
transitionVirusCount fmsg@(FramedMessage fr msg) s = case s of
	Init -> (,mempty) $ case msg of
		Boring Cutscene -> Init
		Boring Pause -> Init
		Boring Transition -> Init
		Boring Glitch -> Init
		Play1{} -> Init
		_ -> Ready
	Ready -> case msg of
		Play1 status -> reportStatus
			(status { viruses = virusesFor (level status) })
			(WrongVirusCount (viruses status))
		_ -> (Ready, mempty)
	WrongVirusCount v -> (,mempty) $ case msg of
		Play1 status
			| viruses status == v -> s
			| otherwise -> Filling (virusesFor (level status))
		Boring Boot -> Ready
		-- no other transitions should ever happen
		_ -> error "Ready"
	Filling v -> (,mempty) $ case msg of
		Play1 status
			| viruses status == v -> Playing v
			| otherwise -> s
		Boring Boot -> Ready
		-- no other transitions should ever happen
		_ -> error "Ready"
	Playing v -> case msg of
		Play1 status -> case compare v (viruses status) of
			LT -> let v' = virusesFor (level status)
			      in reportStatus status { viruses = v' } (Filling v')
			EQ -> (s, mempty)
			GT -> reportStatus status (Playing (viruses status))
		LevelSelect1 _ 0 _ -> stop fr
		LevelSelect1 _ lev _ -> (Restarting lev, mempty)
		Boring msg' -> (,mempty) $ case msg' of
			Boot -> ToppedOut
			-- these next two should never happen
			StartScreen -> error "ToppedOut"
			Demo -> error "ToppedOut"
			_ -> s
		-- no other transitions should ever happen
		_ -> error "ToppedOut"
	ToppedOut -> case msg of
		LevelSelect1 _ 0 _ -> stop fr
		LevelSelect2 _ 0 _ _ _ -> stop fr
		LevelSelect1 _ lev _ -> (Restarting lev, mempty)
		LevelSelect2 _ lev _ _ _ -> (Restarting lev, mempty)
		_ -> (s, mempty)
	Restarting lev -> case msg of
		Boring _ -> (s, mempty)
		LevelSelect1 _ lev' _
			| lev == lev' -> (s, mempty)
			| otherwise -> stop fr
		LevelSelect2{} -> (s, mempty)
		Play1 status -> reportStatus
			(status { viruses = virusesFor (level status) })
			(WrongVirusCount (viruses status))
		Play2{} -> (s, mempty)
	where
	reportStatus status = report fr (printf "%d(%d)" (level status) (viruses status))

transitionScore :: FramedMessage -> State -> (State, ByteString)
transitionScore fmsg@(FramedMessage fr msg) s = case s of
	Init -> (,mempty) $ case msg of
		Boring Cutscene -> Init
		Boring Pause -> Init
		Boring Transition -> Init
		Boring Glitch -> Init
		Play1{} -> Init
		_ -> Ready
	Ready -> case msg of
		Play1 status -> reportStatus 0
			(status { viruses = virusesFor (level status) })
			(Playing (extra status))
		_ -> (Ready, mempty)
	WrongVirusCount score -> unused "WrongVirusCount"
	Filling score -> unused "Filling"
	Playing score -> case msg of
		Boring Cutscene -> (s, mempty)
		Boring Pause -> (s, mempty)
		Boring Transition -> (s, mempty)
		Boring Glitch -> (s, mempty)
		Play1 status -> case compare score (extra status) of
			LT -> reportStatus score status (Playing (extra status))
			EQ -> (s, mempty)
			GT -> error "impossible"
		_ -> stop fr
	ToppedOut -> unused "ToppedOut"
	Restarting v -> unused "Restarting"
	where
	unused s = error (s ++ " unused in transitionScore")
	granularity = 10000
	reportStatus oldScore status s' =
		( s'
		, mconcat
			$ eventMessage fr (show (extra status))
			: [ eventMessage fr (show roundScore ++ "+")
			  | let lowerBound = granularity * (1 + oldScore `div` granularity)
			  , roundScore <- [ lowerBound
			                  , lowerBound + granularity
			                  .. extra status
			                  ]
			  ]
		)

stop :: Word -> (State, ByteString)
stop fr = report fr "STOP" Ready

report :: Word -> String -> State -> (State, ByteString)
report fr s = (, eventMessage fr s)

eventMessage :: Word -> String -> ByteString
eventMessage fr s = BS.pack (printf "%d %s\n" fr s)

main :: IO ()
main = do
	args <- getArgs
	let mode = case args of
	    	[] -> VirusCount
	    	["--score"] -> Score
	    	["--virus"] -> VirusCount
	    	_ -> error "dunno, lol. try --score or --virus instead"
	hSetBuffering stdout LineBuffering
	mainLoop mode Init

mainLoop :: Mode -> State -> IO a
mainLoop mode = go where
	transition = case mode of
		Score -> transitionScore
		VirusCount -> transitionVirusCount
	go s = do
		bs <- BS.getLine
		case A.parseOnly parser bs of
			Left err -> do
				hPutStr stderr "Ignoring line: "
				BS.hPutStr stderr bs
				hPutStrLn stderr (" (" ++ err ++ ")")
				go s
			Right fmsg -> let (s', o) = transition fmsg s in do
				BS.putStr o
				go s'
