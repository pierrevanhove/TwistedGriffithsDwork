SetOptions[$FrontEnd, EvaluationCompletionAction -> "ShowTiming"]
<< FiniteFlow`
<< "/Users/pierre/Git/TwistedGriffithsDwork/Mathematica/Routines-GD.m"

fdoublebox=-(s*x[1]*x[2]*x[4]) - s*x[1]*x[2]*x[5] - s*x[1]*x[4]*x[5] - (-s - t)*x[1]*x[4]*x[5] - 
 t*x[1]*x[4]*x[5] - s*x[2]*x[4]*x[5] - (-s - t)*x[2]*x[4]*x[5] - t*x[2]*x[4]*x[5] - 
 s*x[3]*x[4]*x[5] - (-s - t)*x[3]*x[4]*x[5] - t*x[3]*x[4]*x[5] - s*x[1]*x[2]*x[6] - 
 s*x[1]*x[4]*x[6] - s*x[2]*x[4]*x[6] - s*x[3]*x[4]*x[6] - s*x[1]*x[2]*x[7] - 
 s*x[2]*x[4]*x[7] - s*x[1]*x[5]*x[7] - (-s - t)*x[1]*x[5]*x[7] - t*x[1]*x[5]*x[7] - 
 t*x[3]*x[5]*x[7] - s*x[4]*x[5]*x[7] - (-s - t)*x[4]*x[5]*x[7] - t*x[4]*x[5]*x[7] - 
 s*x[1]*x[6]*x[7] - s*x[4]*x[6]*x[7]/.s->1/.t->X;

udoublebox=x[1]*x[4] + x[2]*x[4] + x[3]*x[4] + x[1]*x[5] + x[2]*x[5] + x[3]*x[5] + x[1]*x[6] + 
 x[2]*x[6] + x[3]*x[6] + x[1]*x[7] + x[2]*x[7] + x[3]*x[7] + x[4]*x[7] + x[5]*x[7] + 
 x[6]*x[7];


var = Variables[udoublebox];

Kinematics = Complement[Variables[fdoublebox], var];

varlong = Join[Join[{\[Epsilon]}, Kinematics], var];

nedges = Length[var]

loop = DegreeHomogeneity[udoublebox, var]

Dim = 4 - 2* \[Epsilon]

\[Delta] = Dim/2 /. \[Epsilon] -> 0

\[Omega] = nedges - loop*Dim/2;

Omegadoublebox = 
 udoublebox^(\[Omega] - Dim/2)/fdoublebox^\[Omega] // Simplify;

Twist = Omegadoublebox/(Omegadoublebox /. \[Epsilon] -> 0) // Simplify;

DegreeHomogeneity[Twist, var]

listderivative = 
 Reverse[Table[
   D[Omegadoublebox, {X, i}]/Omegadoublebox // Simplify // Numerator, {i, 
    2}]]

listdegree = DegreeHomogeneity[listderivative, var]

powerU = 
 Coefficient[FullSimplify[PowerExpand[Log[Omegadoublebox]]], 
  FullSimplify[Log[udoublebox]]]


powerF = -Coefficient[FullSimplify[PowerExpand[Log[Omegadoublebox]]], 
   FullSimplify[Log[fdoublebox]]]


derivativeorder = 2
Print["---- Derivative order ",derivativeorder];

RedFStep1 = 
  ReductionF[listderivative[[1]], listdegree[[1]], 0, 0, 
   Subscript[\[Lambda], 1], fdoublebox, var, varlong];

 RedUStep1 = 
  ReductionU[RedFStep1[[1]], RedFStep1[[2]], Subscript[Q, 1], 
   udoublebox, var, varlong];

 Mstep1 = 
  Mroutine[ RedUStep1[[1]], RedUStep1[[3]], 
    derivativeorder, powerU, powerF,  var] // Expand;


derivativeorder = 1
Print["---- Derivative order ",derivativeorder];


  RedFStep2 = 
  ReductionF[Mstep1, listdegree[[2]], listderivative[[2]], Subscript[
   c, 1], Subscript[\[Lambda], 2], fdoublebox, var, varlong];

RedUStep2 = 
  ReductionU[RedFStep2[[1]], RedFStep2[[2]], Subscript[Q, 2], 
   udoublebox, var, varlong];

 Mstep2 = 
  Mroutine[ RedUStep2[[1]], RedUStep2[[3]], 
    derivativeorder, powerU, powerF, var]//Expand;

Save["Result.txt",{RedFStep1,RedUStep1,Mstep1,RedFStep2,RedUStep2,Mstep2}];
Print["Building the list of coefficients for the PF"];
listPFcoefficient={-Mstep2,Subscript[c,1],1}/.RedFStep1[[4]]/.RedUStep1[[4]]/.RedFStep2[[4]]/.RedUStep2[[4]]//Simplify
Print["Building the inhomogeneous terms"];
Btotal0 = (Bvec[RedUStep1[[1]], 2, fdoublebox, nedges, loop, \[Delta], \[Epsilon]] + Bvec[RedUStep2[[1]], 1, fdoublebox, nedges, loop, \[Delta], \[Epsilon]])//. RedFStep2[[4]] //.  RedUStep2[[4]];
Btotalvar=Complement[Variables[Btotal0],varlong]
Print["Number free variables ",Length[Btotalvar]];
varlongnum=Table[xxx->RandomPrime[{1,100}],{xxx,varlong}];
For[i=1,i<=Length[Btotalvar],i++,{Print["variables ",i,Simplify[Coefficient[Btotal0,Btotalvar[[i]]]]];}]
Btotal=Btotal0/.Table[xxx->0,{xxx,Btotalvar}];

filename="double-box-GD-pf.txt";
Print["saving coefficient PF and boundary terms in ",filename];
Save[filename,{listPFcoefficient,Btotal0,Btotal,Omegadoublebox}];

CheckPF=Sum[D[Omegadoublebox, {X, i}]*listPFcoefficient[[i + 1]], {i, 0, 2}] // Simplify;

dBtotal = Sum[D[Btotal[[i]], var[[i]]], {i, Length[var]}];
Print["Checking the result"];
sumcheck=dBtotal  + CheckPF;
numvarlong=Table[xxx->RandomPrime[{1,200}],{xxx,varlong}];
sumchecknum=sumcheck/.numvarlong;
sumchecknum//Simplify
