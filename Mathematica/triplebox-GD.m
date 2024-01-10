<< FiniteFlow`
<< "Routines-GD.m"

ftriplebox= -(x[1]*x[2]*x[3]*x[5]) - x[1]*x[2]*x[3]*x[6] - x[2]*x[3]*x[4]*x[6] - 
 x[1]*x[3]*x[5]*x[6] - x[2]*x[3]*x[5]*x[6] - x[1]*x[2]*x[3]*x[7] - 
 (-1 - X)*x[1]*x[2]*x[3]*x[7] - X*x[1]*x[2]*x[3]*x[7] - x[2]*x[3]*x[4]*x[7] - 
 (-1 - X)*x[2]*x[3]*x[4]*x[7] - X*x[2]*x[3]*x[4]*x[7] - x[1]*x[2]*x[5]*x[7] - 
 x[2]*x[3]*x[5]*x[7] - (-1 - X)*x[2]*x[3]*x[5]*x[7] - X*x[2]*x[3]*x[5]*x[7] - 
 x[1]*x[2]*x[6]*x[7] - x[1]*x[3]*x[6]*x[7] - (-1 - X)*x[1]*x[3]*x[6]*x[7] - 
 X*x[1]*x[3]*x[6]*x[7] - x[2]*x[4]*x[6]*x[7] - x[3]*x[4]*x[6]*x[7] - 
 (-1 - X)*x[3]*x[4]*x[6]*x[7] - X*x[3]*x[4]*x[6]*x[7] - x[1]*x[5]*x[6]*x[7] - 
 x[2]*x[5]*x[6]*x[7] - x[3]*x[5]*x[6]*x[7] - (-1 - X)*x[3]*x[5]*x[6]*x[7] - 
 X*x[3]*x[5]*x[6]*x[7] - x[1]*x[3]*x[5]*x[8] - x[2]*x[3]*x[5]*x[8] - 
 x[1]*x[3]*x[6]*x[8] - x[2]*x[3]*x[6]*x[8] - x[1]*x[3]*x[7]*x[8] - 
 (-1 - X)*x[1]*x[3]*x[7]*x[8] - X*x[1]*x[3]*x[7]*x[8] - x[2]*x[3]*x[7]*x[8] - 
 (-1 - X)*x[2]*x[3]*x[7]*x[8] - X*x[2]*x[3]*x[7]*x[8] - x[3]*x[4]*x[7]*x[8] - 
 (-1 - X)*x[3]*x[4]*x[7]*x[8] - X*x[3]*x[4]*x[7]*x[8] - x[1]*x[5]*x[7]*x[8] - 
 x[2]*x[5]*x[7]*x[8] - x[3]*x[5]*x[7]*x[8] - (-1 - X)*x[3]*x[5]*x[7]*x[8] - 
 X*x[3]*x[5]*x[7]*x[8] - x[1]*x[6]*x[7]*x[8] - x[2]*x[6]*x[7]*x[8] - 
 x[3]*x[6]*x[7]*x[8] - (-1 - X)*x[3]*x[6]*x[7]*x[8] - X*x[3]*x[6]*x[7]*x[8] - 
 x[1]*x[2]*x[5]*x[9] - x[1]*x[3]*x[5]*x[9] - x[1]*x[2]*x[6]*x[9] - x[1]*x[3]*x[6]*x[9] - 
 x[2]*x[4]*x[6]*x[9] - x[3]*x[4]*x[6]*x[9] - x[1]*x[5]*x[6]*x[9] - x[2]*x[5]*x[6]*x[9] - 
 x[3]*x[5]*x[6]*x[9] - x[1]*x[2]*x[7]*x[9] - (-1 - X)*x[1]*x[2]*x[7]*x[9] - 
 X*x[1]*x[2]*x[7]*x[9] - x[1]*x[3]*x[7]*x[9] - (-1 - X)*x[1]*x[3]*x[7]*x[9] - 
 X*x[1]*x[3]*x[7]*x[9] - x[2]*x[4]*x[7]*x[9] - (-1 - X)*x[2]*x[4]*x[7]*x[9] - 
 X*x[2]*x[4]*x[7]*x[9] - x[3]*x[4]*x[7]*x[9] - (-1 - X)*x[3]*x[4]*x[7]*x[9] - 
 X*x[3]*x[4]*x[7]*x[9] - x[1]*x[5]*x[7]*x[9] - x[2]*x[5]*x[7]*x[9] - 
 (-1 - X)*x[2]*x[5]*x[7]*x[9] - X*x[2]*x[5]*x[7]*x[9] - x[3]*x[5]*x[7]*x[9] - 
 (-1 - X)*x[3]*x[5]*x[7]*x[9] - X*x[3]*x[5]*x[7]*x[9] - x[1]*x[5]*x[8]*x[9] - 
 x[2]*x[5]*x[8]*x[9] - x[3]*x[5]*x[8]*x[9] - x[1]*x[6]*x[8]*x[9] - x[2]*x[6]*x[8]*x[9] - 
 x[3]*x[6]*x[8]*x[9] - x[1]*x[7]*x[8]*x[9] - (-1 - X)*x[1]*x[7]*x[8]*x[9] - 
 X*x[1]*x[7]*x[8]*x[9] - x[2]*x[7]*x[8]*x[9] - (-1 - X)*x[2]*x[7]*x[8]*x[9] - 
 X*x[2]*x[7]*x[8]*x[9] - x[3]*x[7]*x[8]*x[9] - (-1 - X)*x[3]*x[7]*x[8]*x[9] - 
 X*x[3]*x[7]*x[8]*x[9] - X*x[4]*x[7]*x[8]*x[9] - x[1]*x[2]*x[3]*x[10] - 
 x[2]*x[3]*x[4]*x[10] - x[1]*x[2]*x[5]*x[10] - x[2]*x[3]*x[5]*x[10] - 
 x[1]*x[2]*x[6]*x[10] - x[1]*x[3]*x[6]*x[10] - x[2]*x[4]*x[6]*x[10] - 
 x[3]*x[4]*x[6]*x[10] - x[1]*x[5]*x[6]*x[10] - x[2]*x[5]*x[6]*x[10] - 
 x[3]*x[5]*x[6]*x[10] - x[1]*x[3]*x[8]*x[10] - x[2]*x[3]*x[8]*x[10] - 
 x[3]*x[4]*x[8]*x[10] - x[1]*x[5]*x[8]*x[10] - x[2]*x[5]*x[8]*x[10] - 
 x[3]*x[5]*x[8]*x[10] - x[1]*x[6]*x[8]*x[10] - x[2]*x[6]*x[8]*x[10] - 
 x[3]*x[6]*x[8]*x[10] - x[1]*x[2]*x[9]*x[10] - x[1]*x[3]*x[9]*x[10] - 
 x[2]*x[4]*x[9]*x[10] - x[3]*x[4]*x[9]*x[10] - x[1]*x[5]*x[9]*x[10] - 
 x[2]*x[5]*x[9]*x[10] - x[3]*x[5]*x[9]*x[10] - x[1]*x[8]*x[9]*x[10] - 
 x[2]*x[8]*x[9]*x[10] - x[3]*x[8]*x[9]*x[10];

utriplebox=x[1]*x[2]*x[3] + x[2]*x[3]*x[4] + x[2]*x[3]*x[5] + x[1]*x[3]*x[6] + x[3]*x[4]*x[6] + 
 x[3]*x[5]*x[6] + x[1]*x[2]*x[7] + x[2]*x[4]*x[7] + x[2]*x[5]*x[7] + x[1]*x[6]*x[7] + 
 x[4]*x[6]*x[7] + x[5]*x[6]*x[7] + x[1]*x[3]*x[8] + x[2]*x[3]*x[8] + x[3]*x[4]*x[8] + 
 x[3]*x[5]*x[8] + x[3]*x[6]*x[8] + x[1]*x[7]*x[8] + x[2]*x[7]*x[8] + x[4]*x[7]*x[8] + 
 x[5]*x[7]*x[8] + x[6]*x[7]*x[8] + x[1]*x[2]*x[9] + x[1]*x[3]*x[9] + x[2]*x[4]*x[9] + 
 x[3]*x[4]*x[9] + x[2]*x[5]*x[9] + x[3]*x[5]*x[9] + x[1]*x[6]*x[9] + x[4]*x[6]*x[9] + 
 x[5]*x[6]*x[9] + x[1]*x[7]*x[9] + x[4]*x[7]*x[9] + x[5]*x[7]*x[9] + x[1]*x[8]*x[9] + 
 x[2]*x[8]*x[9] + x[3]*x[8]*x[9] + x[4]*x[8]*x[9] + x[5]*x[8]*x[9] + x[6]*x[8]*x[9] + 
 x[7]*x[8]*x[9] + x[1]*x[2]*x[10] + x[2]*x[4]*x[10] + x[2]*x[5]*x[10] + 
 x[1]*x[6]*x[10] + x[4]*x[6]*x[10] + x[5]*x[6]*x[10] + x[1]*x[8]*x[10] + 
 x[2]*x[8]*x[10] + x[4]*x[8]*x[10] + x[5]*x[8]*x[10] + x[6]*x[8]*x[10] + 
 x[1]*x[9]*x[10] + x[4]*x[9]*x[10] + x[5]*x[9]*x[10] + x[8]*x[9]*x[10];


var = Variables[utriplebox];

Kinematics = Complement[Variables[ftriplebox], var];

varlong = Join[Join[{\[Epsilon]}, Kinematics], var];

nedges = Length[var]

loop = DegreeHomogeneity[utriplebox, var]

Dim = 4 - 2* \[Epsilon]

\[Delta] = Dim/2 /. \[Epsilon] -> 0

\[Omega] = nedges - loop*Dim/2;

Omegatriplebox = 
 utriplebox^(\[Omega] - Dim/2)/ftriplebox^\[Omega] // Simplify;

Twist = Omegatriplebox/(Omegatriplebox /. \[Epsilon] -> 0) // Simplify;

DegreeHomogeneity[Twist, var]

listderivative = 
 Reverse[Table[
   D[Omegatriplebox, {X, i}]/Omegatriplebox // Simplify // Numerator, {i, 
    3}]]

listdegree = DegreeHomogeneity[listderivative, var]

derivativeorder = 3
Print["---- Derivative order ",derivativeorder];

RedFStep1 = 
  ReductionF[listderivative[[1]], listdegree[[1]], 0, 0, 
   Subscript[\[Lambda], 1], ftriplebox, var, varlong];

 RedUStep1 = 
  ReductionU[RedFStep1[[1]], RedFStep1[[2]], Subscript[Q, 1], 
   utriplebox, var, varlong];

 Mstep1 = 
  Mroutine[ RedUStep1[[1]], RedUStep1[[3]], 
    derivativeorder, loop, \[Delta], \[Epsilon], var] // Expand;


derivativeorder = 2
Print["---- Derivative order ",derivativeorder];


  RedFStep2 = 
  ReductionF[Mstep1, listdegree[[2]], listderivative[[2]], Subscript[
   c, 2], Subscript[\[Lambda], 2], ftriplebox, var, varlong];

RedUStep2 = 
  ReductionU[RedFStep2[[1]], RedFStep2[[2]], Subscript[Q, 2], 
   utriplebox, var, varlong];

 Mstep2 = 
  Mroutine[ RedUStep2[[1]], RedUStep2[[3]], 
    derivativeorder, loop, \[Delta], \[Epsilon], var]//Expand;

derivativeorder = 1
Print["---- Derivative order ",derivativeorder];


  RedFStep3 = 
  ReductionF[Mstep2, listdegree[[3]], listderivative[[3]], Subscript[
   c, 1], Subscript[\[Lambda], 3], ftriplebox, var, varlong];

RedUStep3 = 
  ReductionU[RedFStep3[[1]], RedFStep3[[2]], Subscript[Q, 3], 
   utriplebox, var, varlong];

 Mstep3 = 
  Mroutine[ RedUStep3[[1]], RedUStep3[[3]], 
    derivativeorder, loop, \[Delta], \[Epsilon], var]//Expand;


Save["Result.txt",{RedFStep1,RedUStep1,Mstep1,RedFStep2,RedUStep2,Mstep2,RedFStep3,RedUStep3,Mstep3}];
Print["Building the list of coefficients for the PF"];
listPFcoefficient={-Mstep3,Subscript[c,1],Subscript[c,2],1}/.RedFStep1[[4]]/.RedUStep1[[4]]/.RedFStep2[[4]]/.RedUStep2[[4]]/.RedFStep3[[4]]/.RedUStep3[[4]]//Simplify
filename="triple-box-GD-pf.txt";
Print["saving coefficient PF in ",filename];
Save[filename,{listPFcoefficient,Omegatriplebox}];

Quit

