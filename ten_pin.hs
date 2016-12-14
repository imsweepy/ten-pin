--list used for first 9 frames and tuple used for last frame
--first 9 frames are stored reversed to have O(1) insert and read
data Score_card = Card [(Maybe Int, Maybe Int)] (Maybe Int, Maybe Int, Maybe Int)
    deriving (Show, Eq)

--tests
test_1 = update_card 1 create_card == Card [(Just 1, Nothing)] (Nothing, Nothing, Nothing)
test_2 = Card [(Just 1, Just 2)] (Nothing, Nothing, Nothing) == update_card 2 (update_card 1 create_card)
test_3 = Card [(Just 10, Nothing),(Just 1, Just 2)] (Nothing, Nothing, Nothing) == update_card 10 (update_card 2 (update_card 1 create_card))
test_4 = Card [(Just 7, Nothing),(Just 10, Nothing),(Just 1, Just 2)] (Nothing, Nothing, Nothing) == update_card 7 (update_card 10 (update_card 2 (update_card 1 create_card)))
test_5 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Nothing, Nothing) == update_card 2 (Card (take 9 (repeat (Just 1,Just 8))) (Nothing , Nothing, Nothing))
test_6 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Nothing) == update_card 8 (Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Nothing, Nothing))
test_7 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Just 10) == update_card 10 (Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Nothing))
test_8 = do
    new_game_score <- tally_score_card create_card
    return $new_game_score == 0
test_9 = do
    perfect_game_score <- tally_score_card $Card (take 9 (repeat (Just 10,Nothing ))) (Just 10, Just 10, Just 10)
    return $perfect_game_score == 300
test_10 = do
    card_score <- tally_score_card $Card [(Just 1, Just 8)] (Nothing, Nothing, Nothing)
    return $card_score == 9
test_11 = do
    card_score <- tally_score_card $Card (take 9 (repeat (Just 1,Just 8))) (Nothing, Nothing, Nothing)
    return $card_score == 9*9
test_12 = do
    card_score <- tally_score_card $Card (reverse((take 5 (repeat (Just 1,Just 8)))++(take 4 (repeat (Just 1,Just 5))))) (Nothing, Nothing, Nothing)
    return $card_score == 9*5+6*4
test_13 = do
    card_score <- tally_score_card $Card (reverse[(Just 10, Nothing),(Just 1,Just 2),(Just 3,Just 4)]) (Nothing, Nothing, Nothing)
    return $card_score == 10+1+1+2+2+3+4
test_14 = do
    card_score <- tally_score_card $Card (reverse[(Just 10, Nothing),(Just 1,Just 2),(Just 3,Just 7),(Just 4,Just 2)]) (Nothing, Nothing, Nothing)
    return $card_score == 10+1+1+2+2+3+7+4+4+2
test_15 = do
    card_score <- tally_score_card $Card (reverse[(Just 10, Nothing),(Just 10, Nothing),(Just 3,Just 7),(Just 4,Just 2)]) (Nothing, Nothing, Nothing)
    return $card_score == 10+10+10+3+3+3+7+7+4+4+2

create_card :: Score_card
create_card = Card [] (Nothing, Nothing, Nothing)

--add bowls to score card
--assumes frames in score card are valid
update_card :: Int -> Score_card -> Score_card
update_card bowl (Card first_9_frames final_frame) = if bowl>10 || bowl<0
    then error "invalid bowl"
    else if first_9_frames == []
        then Card [(Just bowl, Nothing)] final_frame
        else if length first_9_frames == 9
            then update_final_frame bowl (Card first_9_frames final_frame) 
            else if new_frame
                then Card ((Just bowl, Nothing):first_9_frames) final_frame
                else Card ((fst (head first_9_frames), Just bowl):(tail first_9_frames)) final_frame
                    where new_frame = need_new_frame bowl $head first_9_frames
 
--check if a bowl is part of new frame or part of last frame
need_new_frame :: Int -> (Maybe Int, Maybe Int) -> Bool
need_new_frame bowl last_frame = case last_frame of
    (Just x, Just y)  -> True
    (Just x, Nothing) -> if x == 10
        then True
        else if x+bowl>10
            then error "invalid bowl for score card"
            else False
    (Nothing, Nothing) -> error "invalid score card"


update_final_frame :: Int -> Score_card -> Score_card
update_final_frame bowl (Card first_9_frames final_frame) = case final_frame of
    (Nothing, Nothing, Nothing) -> Card first_9_frames (Just bowl, Nothing, Nothing)
    (Just x, Nothing, Nothing) -> if x== 10 || x+bowl==10
        then Card first_9_frames (Just x, Just bowl, Nothing)
        else error "invalid bowl"
    (Just x, Just y, Nothing) -> if x== 10 || x+y==10
        then Card first_9_frames (Just x, Just y, Just bowl)
        else error "game should have ended already or invalid score card"
    _                         -> error "game should have ended already or invalid score card"

tally_score_card :: Score_card -> IO(Int)
tally_score_card (Card first_9_frames final_frame) = if length first_9_frames == 9
    then case final_frame of
        (Nothing, Nothing, Nothing) -> do
            putStrLn "game not finished"
            return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
        (Just x, Nothing, Nothing) -> do
            putStrLn "game not finished"
            return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
        (Just x, Just y, Nothing) -> if x+y>=10 
            then do
                putStrLn "game not finished"
                return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
            else do
                putStrLn "game finished"
                return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
        (Just x, Just y, Just z) -> do
            putStrLn "game finished"
            return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
    else if length first_9_frames < 9 
        then do
            putStrLn "game not finished"
            return $tally_score_card' (reverse first_9_frames) final_frame 0 0 0
        else error "error invalid score card"

tally_score_card' :: [(Maybe Int, Maybe Int)] -> (Maybe Int, Maybe Int, Maybe Int) -> Int -> Int -> Int -> Int
tally_score_card' first_9_frames final_frame last_spare last_strike last_last_strike = case first_9_frames of
    []   -> tally_final_frame final_frame last_spare last_strike last_last_strike
    x:xs -> case x of
        (Just 10, Nothing) -> 10*(1+last_spare+last_strike+last_last_strike) + tally_score_card' xs final_frame 0 1 last_strike
        (Just x, Just y) -> case x+y of
            10 -> x*(1+last_spare+last_strike+last_last_strike) + y*(1+last_strike) + tally_score_card' xs final_frame 1 0 0
            _  -> x*(1+last_spare+last_strike+last_last_strike) + y*(1+last_strike) + tally_score_card' xs final_frame 0 0 0

tally_final_frame :: (Maybe Int, Maybe Int, Maybe Int) -> Int -> Int -> Int -> Int
tally_final_frame final_frame last_spare last_strike last_last_strike = case final_frame of
    (Nothing, Nothing, Nothing) -> 0
    (Just x, Nothing, Nothing) -> x*(1+last_spare+last_strike+last_last_strike)
    (Just x, Just y, Nothing) -> x*(1+last_spare+last_strike+last_last_strike) + y*(1+last_strike)
    (Just x, Just y, Just z) -> x*(1+last_spare+last_strike+last_last_strike) + y*(1+last_strike) + z
