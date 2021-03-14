-- sequenceSum(2,2,2) === 2
-- sequenceSum(2,6,2) === 12 // 2 + 4 + 6
-- sequenceSum(1,5,1) === 15 // 1 + 2 + 3 + 4 + 5
-- sequenceSum(1,5,3) === 5 // 1 + 4

sequences (start, end, step) = summ (myseq (start, end, step))

myseq (st,0,step) = []
myseq (st,en,step)
    | st <= en =  st :  myseq (st+step, en, step)
    | otherwise = myseq (st, en-1, step)



summ []     = 0
summ (x:xs) = x + summ xs 