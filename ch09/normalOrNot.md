1. [1, 2, 3, 4, 5] 
*NF and WHNF*

2. 1 : 2 : 3 : 4 : _
*WHNF*

3. enumFromTo 1 10 
*Neither. Function and arguments fully applied but not evaluated*

4. length [1, 2, 3, 4, 5] 
*Neither. Function and arguments fully applied but not evaluated*

5. sum (enumFromTo 1 10) 
*Neither. Function and arguments fully applied but not evaluated*

6. ['a'..'m'] ++ ['n'..'z'] 
*Neither. Function and arguments fully applied but not evaluated*

7. (_, 'b')
*WHNF*
