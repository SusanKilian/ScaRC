

base = '/home/susanne/GIT/A_ScaRC/Verification/MGM/matlab/'
matrix = "lup1.txt"

mat = base + matrix;

%
% LU-decomposition of structured A
%
A = dlmread(mat);
[L,U] = lu(A);

b = [ 
       1.0; 1.0; 1.0; 1.0; 1.0;  
       1.0; 1.0; 1.0; 1.0; 1.0;
       1.0; 1.0;      1.0; 1.0;  
       1.0; 1.0; 1.0; 1.0; 1.0;
       1.0; 1.0; 1.0; 1.0; 1.0;  
    ];
 

x1 = A\b; 

y = L\b;
x2 = U\y;

x3 = x1-x2

