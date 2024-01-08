<< FiniteFlow`
<< "/Users/pierre/Git/TwistedGriffithsDwork/Mathematica/Routines-GD.m"

Uicecream=x[2]*(x[3] + x[4]) + x[1]*(x[2] + x[3] + x[4]);
Ficecream2=x[2]*x[3]*x[4] + x[1]*(x[3]*x[4] + x[2]*(u*x[3] + v*x[4]));
IntIceCream=(x[1]^\[Kappa]*x[2]^\[Kappa]*x[3]^\[Kappa]*x[4]^\[Kappa]*(x[2]*(x[3] + x[4]) + x[1]*(x[2] + x[3] + x[4]))^(-2 + 4*\[Kappa]))/(x[2]*x[3]*x[4] + x[1]*(x[3]*x[4] + x[2]*(u*x[3] + v*x[4])))^(4*\[Kappa])
var = Sort[Variables[Uicecream]]
Kinematics = Complement[Variables[Ficecream2], Variables[Uicecream]]
varlong = Join[{\[Kappa]}, Kinematics, var]
listderivativeu = Reverse[Table[ Numerator[Simplify[D[IntIceCream, {u, i}]/IntIceCream]], {i, 1, 3}]];
listdegreeu = DegreeHomogeneity[listderivativeu, var]
listderivativev = Reverse[Table[ Numerator[Simplify[D[IntIceCream, {v, i}]/IntIceCream]], {i, 1, 3}]];
listdegreev = DegreeHomogeneity[listderivativev, var]
listderivativeuv = {Numerator[ Simplify[D[D[IntIceCream, {v, 1}], {u, 1}]/IntIceCream]]};
listdegreeuv = DegreeHomogeneity[listderivativeuv, var]

powerU = -2 + 4*\[Kappa]

powerF = 4*\[Kappa]

powerQ = \[Kappa]

derivativeorder=3
RedFStep1=ReductionF[listderivativeu[[1]],listdegreeu[[1]],0,0,Subscript[\[Lambda],1],Ficecream2,var,varlong];
RedUStep1=ReductionU[RedFStep1[[1]],RedFStep1[[2]],Subscript[Q, 1],Uicecream,var,varlong];
solnRstep1=ReductionU[RedUStep1[[1]],RedFStep1[[2]],Subscript[Q, 11],x[1]*x[2]*x[3]*x[4],var,varlong];
Mstep1=(Sum[D[RedUStep1[[1]][[i]],var[[i]]],{i,Length[var]}]+powerU*RedUStep1[[3]]+powerQ*solnRstep1[[3]] )/(derivativeorder-1+powerF)/.solnRstep1[[4]];

derivativeorder=2
RedFStep2=ReductionF[Mstep1,listdegreev[[2]],listderivativev[[2]],Subscript[c, 2 v],Subscript[\[Lambda], 2],Ficecream2,var,varlong];
RedUStep2=ReductionU[RedFStep2[[1]],RedFStep2[[2]],Subscript[Q, 2],Uicecream,var,varlong];
solnRstep2=ReductionU[RedUStep2[[1]],RedFStep2[[2]],Subscript[Q, 22],x[1]*x[2]*x[3]*x[4],var,varlong];
Mstep2=(Sum[D[solnRstep2[[1]][[i]],var[[i]]],{i,Length[var]}]+powerU*RedUStep2[[3]]+powerQ*solnRstep2[[3]])/(derivativeorder-1+powerF)/.solnRstep2[[4]];

derivativeorder=1
RedFStep3=ReductionF[Mstep2,listdegreev[[3]],{listderivativev[[3]],listderivativeu[[3]]},{Subscript[c, 1 v],Subscript[c, 1 u]},Subscript[\[Lambda], 3],Ficecream2,var,varlong];
RedUStep3=ReductionU[RedFStep3[[1]],RedFStep3[[2]],Subscript[Q, 3],Uicecream,var,varlong];
solnRstep3=ReductionU[RedUStep3[[1]],RedFStep3[[2]],Subscript[Q, 32],x[1]*x[2]*x[3]*x[4],var,varlong];
Mstep3=(Sum[D[solnRstep3[[1]][[i]],var[[i]]],{i,Length[var]}]+powerU*RedUStep3[[3]]+powerQ*solnRstep3[[3]])/(derivativeorder-1+powerF)/.solnRstep3[[4]];

listPF3={-Mstep3,Subscript[c, 1u],Subscript[c, 1 v],Subscript[c, 2 v],1}//.RedFStep1[[4]]//.RedUStep1[[4]]//.solnRstep1[[4]]//.RedFStep2[[4]]//.RedUStep2[[4]]//.solnRstep2[[4]]//.RedFStep3[[4]]//.RedUStep3[[4]]//.solnRstep3[[4]]//Simplify
