(* ::Package:: *)

(*Load FiniteFlow*)
<<FiniteFlow`

(*Finite flow*)
solsFF[coeffs__,vars__]:=Module[{},
FFDenseSolve[coeffs,vars,"ApplyFunction"->Together,MaxPrimes->100,MaxDegree->200]
]
(*Obtain equations*)
getEquations[pol_,vars__]:=(CoefficientArrays[{pol},vars]["NonzeroValues"])//Through//Flatten;

(*Jacobian ideal as list *)
gradPol[pol_,vars__]:=D[pol,#]&/@vars;

(*compute degree of homegeneity*)
DegreeHomogeneity[f_,xxx__]:=Module[{vartmp,numvartmp,ftmp},{vartmp=Complement[Variables[f],xxx]; numvartmp=Table[vartmp[[i]]->RandomPrime[{1,1000}],{i,Length[vartmp]}];ftmp=f/.numvartmp;Exponent[Simplify[(ftmp/.Table[xxtmp->xxtmp \[Lambda],{xxtmp,xxx}])/ftmp],\[Lambda]]}];

(*generates monomials o given degree*)
allMonoms[n_,deg_,x_]:=DeleteCases[Coefficient[(List@@Expand[(1+Total[Array[x,n]])^deg]/. j_Integer*monom:_:>monom)/.{x[i_]:>\[Lambda] x[i]},\[Lambda]^deg],0];
(*generates homogeneous polynomials of a given degree*)
homPols[n_,deg_,sym_]:=Module[{mons},
mons=If[deg==0,1,allMonoms[n,deg,x]];
Return[If[deg==0,sym,(Plus@@(sym/@((Exponent[#,Array[x,n]]&/@mons))mons ))/.{sym[A__]:>sym[Sequence@@A]}]];
]


(*Symanzik polynomials*)
f=((-t  x[3] x[5] x[7]-s((x[1]+x[2]+x[3])x[4]x[6]+(x[4]+x[5]+x[6])x[1]x[2]+(x[1]+x[4])(x[2]+x[6])x[7]))/.{s->1,t->X})//Simplify
u=x[3] x[4]+x[3] x[5]+x[3] x[6]+x[3] x[7]+x[4] x[7]+x[5] x[7]+x[6] x[7]+x[1] (x[4]+x[5]+x[6]+x[7])+x[2] (x[4]+x[5]+x[6]+x[7])//Simplify;
(*D=2+4eps*)
del=2;
loops=2;
n=7;
vars=Array[x,7];
(*allvariables in the problem*)
varslong=Join[Variables[f],{eps}];
(*Gradients*)
gradU=gradPol[u,vars];
gradF=gradPol[f,vars];

omegaeps=PowerExpand[u^(n-del*loops-del)/f^(n-del*loops)(u^(loops+1)/f^loops)^eps];

(*Order derivatives by decreasing degree*)
dersomegaeps=Reverse[Table[D[omegaeps,{X,i}]/omegaeps//Numerator//Simplify,{i,1,2}]];


lambdaU=n-(loops+1)(del-eps);
lambdaF=n-(loops)(del-eps);

(*First reduction: second order differential *)
labelStep=2;

pol2=dersomegaeps[[1]];
degpol2=labelStep*(loops+1);


(*Vector of polynomials with unknown coefficients C*)
degredF2=degpol2-loops;
C2=Table[homPols[n,degredF2,\[Lambda][labelStep,i]],{i,1,Length[vars]}];

(*polynomial for the reduction of U*)
degredU2=degpol2-loops-1;
c2=homPols[n,degredU2,Q[labelStep]];

(*gnerate a system of equations for the coefficients of C2 and c2*)
eqReductionF2=(pol2-C2 . gradF)//Expand;
eqReductionU2=(C2 . gradU-c2 u)//Expand;
systemF2=getEquations[eqReductionF2,vars];
systemU2=getEquations[eqReductionU2,vars];

alleqs2=Join[systemF2,systemU2];
allvars2=Complement[Variables[alleqs2],varslong];

Print["Solving a system of "<>ToString[Length[alleqs2]]<>" equations and "<>ToString[Length[allvars2]]<>" variables" ];
sols2=solsFF[Equal[#,0]&/@alleqs2,allvars2];
Print["System solved "];

C2sol=C2/.Dispatch[sols2];
c2sol=c2/.Dispatch[sols2];


M2=(lambdaU c2sol+Sum[D[C2sol[[i]],x[i]],{i,n}])/(labelStep-1+lambdaF)//Expand;


(*Second reduction: second order differential *)
labelStep=1;

(*THE SAME PROCEDURE but for a new polynomial*)
pol1=M2+q dersomegaeps[[2]]//Expand;
degpol1=DegreeHomogeneity[pol1,vars][[1]];


(*Vector of polynomials with unknown coefficients C*)
degredF1=degpol1-loops;
C1=Table[homPols[n,degredF1,\[Lambda][labelStep,i]],{i,1,Length[vars]}];

(*polynomial for the reduction of U*)
degredU1=degpol1-loops-1;
c1=homPols[n,degredU1,Q[labelStep]];

(*generate a system of equations for the coefficients of C1 and c1*)
eqReductionF1=(pol1-C1 . gradF)//Expand;
eqReductionU1=(C1 . gradU-c1 u)//Expand;
systemF1=getEquations[eqReductionF1,vars];
systemU1=getEquations[eqReductionU1,vars];

alleqs1=Join[systemF1,systemU1];
allvars1=Complement[Variables[alleqs1],varslong];

Print["Solving a system of "<>ToString[Length[alleqs1]]<>" equations and "<>ToString[Length[allvars1]]<>" variables" ];
sols1=solsFF[Equal[#,0]&/@alleqs1,allvars1];
Print["System solved "];

C1sol=C1/.Dispatch[sols1]/.Dispatch[sols2];
c1sol=c1/.Dispatch[sols1]/.Dispatch[sols2];
qsol=q/.Dispatch[sols1]/.Dispatch[sols2];
M1=(lambdaU c1sol+Sum[D[C1sol[[i]],x[i]],{i,n}])/(labelStep-1+lambdaF)//Expand;


(*Result of the reduction*)
redcoeffs={qsol,-M1};


(*check that the result does not depend on free variables*)
varsfree=Complement[Variables[redcoeffs],varslong];
arraysresults=CoefficientArrays[redcoeffs[[#]],varsfree]&/@Range[2];

(*Check that coefficients of free variables are zero*)
(*firstcoefficient does not depend on free variables*)
arraysresults[[1]]//Length;
(*Coefficients of free variables vanish after simplification*)
DeleteDuplicates[arraysresults[[2]][[2]]//Normal//Simplify]


(*final result*)
coeffs={arraysresults[[1]],arraysresults[[2]][[1]]}//Simplify//Flatten


filename="double-box-GD-DATA.m";
Print["saving data for reduction",filename];
Export[filename,{C1sol,sols1,C2sol,sols2,omegaeps,coeffs}];
