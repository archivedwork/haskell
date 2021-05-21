import Data.List
import Data.Char



test_removeLast = [
    removeLast even [0, 1, 2, 3] == [0, 1, 3], 
    removeLast odd  [0, 1, 2, 3] == [0, 1, 2] ,
    removeLast even [1, 3, 5]    == [1, 3, 5],
    removeLast (> 0) []          == []]

removeLast p lst = reverse $ removeLasthelper p (reverse lst)


removeLasthelper p [] = []
removeLasthelper p lst@(x:xs)
    | p x = xs
    | otherwise = x : removeLasthelper p xs





-- Define a function `duplicateLast :: (a -> Bool) -> [a] -> [a]`
--   `duplicateLast p xs` should duplicate the last element of `xs` that satisfies the predicate `p`.


-- Examples:
--   duplicateLast odd  [1] == [1, 1]
--   duplicateLast even [0, 1, 2, 3] == [0, 1, 2, 2, 3]
--   duplicateLast odd  [0, 1, 2, 3] == [0, 1, 2, 3, 3]
--   duplicateLast odd  [0, 2, 4, 6] == [0, 2, 4, 6]


duplicateLast :: (a -> Bool) -> [a] -> [a]
duplicateLast p lst = reverse $ helper_dup  p (reverse lst)


helper_dup p lst@(x:xs)
    | p x        = (x:x:xs)
    | otherwise  = x : helper_dup p xs
 



--Define a function 'insertAtPred'.
--'insertAtPred p y xs' should insert y before the first element of xs that
--  satisfies the predicate p, or at the end of xs if no element of xs
--  satisfies the predicate p.

--Examples:

test_insertAtPred = [    
    insertAtPred even 99 [1, 3, 5, 2] == [1, 3, 5, 99, 2],
    insertAtPred even 99 [1, 3, 5, 7] == [1, 3, 5, 7, 99],
    insertAtPred odd  99 [1, 3, 5, 2] == [99, 1, 3, 5, 2],
    insertAtPred even 99 []           == [99]]

insertAtPred :: (a -> Bool) -> a -> [a] -> [a]
insertAtPred p n [] = [n]
insertAtPred p n lst@(x:xs)
    | p x         = (n:x:xs)
    | otherwise   = x : insertAtPred p n xs





-- Define a function `duplicatePred :: (a -> Bool) -> [a] -> [a]`
--   `duplicatePred p l` should duplicate the elements of `l` that satisfy the
--   predicate `p`.

 

-- Examples:
test_dup = [
  duplicatePred odd  [1, 2, 3] == [1, 1, 2, 3, 3],
  duplicatePred even [1, 2, 3] == [1, 2, 2, 3],
  duplicatePred odd  [2, 4, 6] == [2, 4, 6]]

duplicatePred p [] = []
duplicatePred p lst@(x:xs)
    | p x       = x : x : duplicatePred p xs
    | otherwise = x : duplicatePred p xs

