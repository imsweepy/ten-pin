--list used for first 9 frames and tuple used for last frame
--first 9 frames are stored reversed to have O(1) insert and read
data Score_card = Card [(Maybe Int, Maybe Int)] (Maybe Int, Maybe Int, Maybe Int)
    deriving (Show, Eq)

create_card :: Score_card
create_card = Card [] (Nothing, Nothing, Nothing)