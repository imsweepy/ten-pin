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