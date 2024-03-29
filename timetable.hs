module Timetable where

data Activity = Lecture | Seminar | Laboratory | Assessment deriving (Show, Eq)
data Period = AM | PM deriving (Show, Eq, Ord)
day :: [Period]
day = [AM, PM]
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Eq)
week :: [Day]
week = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
type Slot = (Day, Period)

class Timetable t where
  --Constructors
  emptyTimetable :: t
  addActivity :: (Slot, Activity) -> t -> t
  clearSlot :: Slot -> t -> t
  --Selectors
  activity :: t->Slot -> [Activity]
  
putTimetable :: Timetable t => t -> IO ()
putTimetable t = putStr(unlines [show d ++ " " ++ show p
                                ++ " " ++ show (activity t (d,p))
                                | d <- week, p <-day])

list2timetable :: Timetable t => [(Slot, Activity)] -> t
list2timetable lst = foldr addActivity emptyTimetable lst

assessmentDays :: Timetable t => t -> [Day]
assessmentDays t = [d|d <- week, Assessment `elem` ((activity t (d, AM)) ++ (activity t (d, PM)))]

timetable2list :: Timetable t => t -> [(Slot, Activity)]
timetable2list t = concat [[((d, p),a) |a <- activity t (d, p)]|d <- week, p <- day]

--freeWedPM, someAssessment, onlyAssessmentSat, noBackToback :: Timetable t => t -> Bool

newtype TRel = TRel [(Slot, Activity)]

instance Timetable TRel where
  emptyTimetable = TRel []
  
  addActivity (s, a) (TRel t) = TRel ((s, a):t)
  
  clearSlot s (TRel ((s', a):xs)) | s' == s = clearSlot s (TRel xs)
                                | otherwise = addActivity (s', a) (clearSlot s (TRel xs))
                                              
  activity (TRel [(s',a')]) s | s == s' = [a']
                              | otherwise = []
  activity (TRel ((s',a'):xs)) s | s == s' = a':(activity (TRel xs) s)
                               | otherwise = activity (TRel xs) s
newtype TFun = TFun {Tfun :: Slot -> [Activity]}
