#CL-POKER-SIM

Simple monte-carlo simulations of texas hold'em poker hands and utilities for calculating positive-potential of given two card hand and board and exhaustive simulation of two card hands.  

Card enumeration used:  

    2c-Ac -> 0-12  
    2d-Ad -> 13-25  
    2h-Ah -> 26-38  
    2s-As -> 39-51  

####monte-carlo-sim (n hole noppts)
Simulates n hands against noppts oppents for given two card hand hole.  Returns values win% tie% lose%.

    CL-POKER-SIM> (monte-carlo-sim 100000 '(12 39) 9)
    CL-POKER-SIM> 0.10308
    CL-POKER-SIM> 0.03208
    CL-POKER-SIM> 0.86484
    
####noppts-sim (n hole board noppts)
Simulates n hands against noppts oppents for give two card hand hole and three card board. Returns values win% tie% lose%.  

    CL-POKER-SIM> (noppts-sim 100000 '(12 39) '(9 24 26) 9)
    CL-POKER-SIM> 0.10308
    CL-POKER-SIM> 0.03208
    CL-POKER-SIM> 0.86484
    
    



    
