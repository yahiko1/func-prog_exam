numbers1 = 1 : map (+ 1) numbers1

main = do
  -- task 2
  print "Task 2"
  print (take 100 numbers1)
  let cum_sum = scanl1 (+) numbers1
  print (take 20 cum_sum)