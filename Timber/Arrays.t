module Arrays where

import POSIX

root env = class
    a := uniarray 10 0
    
    result action
        x = a!0
        env.stdout.write (show x ++ "\n")
        y = a!30
        env.stdout.write (show y ++ "\n")
        
        env.exit(0)