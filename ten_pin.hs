--list used for first 9 frames and tuple used for last frame
--first 9 frames are stored reversed to have O(1) insert and read
data Score_card = Card [(Maybe Int, Maybe Int)] (Maybe Int, Maybe Int, Maybe Int)
    deriving (Show, Eq)

--tests
test_1 = update_card 1 create_card == Card [(Just 1, Nothing)] (Nothing, Nothing, Nothing)
test_2 = Card [(Just 1, Just 2)] (Nothing, Nothing, Nothing) == update_card 2 (update_card 1 create_card)
test_3 = Card [(Just 1, Just 2),(Just 10, Nothing)] (Nothing, Nothing, Nothing) == update_card 10 (update_card 2 (update_card 1 create_card))
test_4 = Card [(Just 1, Just 2),(Just 10, Nothing),(Just 7, Nothing)] (Nothing, Nothing, Nothing) == update_card 7 (update_card 10 (update_card 2 (update_card 1 create_card)))
test_5 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Nothing, Nothing) == update_card 2 (Card (take 9 (repeat (Just 1,Just 8))) (Nothing , Nothing, Nothing))
test_6 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Nothing) == update_card 8 (Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Nothing, Nothing))
test_7 = Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Just 10) == update_card 10 (Card (take 9 (repeat (Just 1,Just 8))) (Just 2, Just 8, Nothing))


create_card :: Score_card
create_card = Card [] (Nothing, Nothing, Nothing)

update_card :: Int -> Score_card -> Score_card
update_card ball card = card