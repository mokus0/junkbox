,>,>++++++[-<--------<-------->>] Store 2 numbers from kb in (0) and (1); and subtract 48 from each
<<[                               Loop until the dividend is zero
>[->+>+<<]                        Move the divisor in (1) to (2) and (3); setting (1) to zero
>[-<<-                            Subtract 1 from both the dividend(0) and the divisor(2) until (2) is zero
[>]>>>[<[>>>-<<<[-]]>>]<<]        If the dividend is zero; exit the loop
>>>+                              Add one to the quotient in (5)
<<[-<<+>>]                        Move the saved divisor in (3) to (2)
<<<]                              Move ptr to {0} and repeat loop
>[-]>>>>[-<<<<<+>>>>>]            Move the quotient in (5) to (0)
<<<<++++++[-<++++++++>]<.         Add 48 and print result
