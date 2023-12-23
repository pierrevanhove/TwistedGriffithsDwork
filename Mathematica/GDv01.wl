(* ::Package:: *)



(*Augmented number of primes*)
solsFF[coeffs__,vars__]:=Module[{},
FFDenseSolve[coeffs,vars,"ApplyFunction"->Together,MaxPrimes->100,MaxDegree->200]
]
JacIdeal[pol_,vars__]:=D[pol,#]&/@vars;

DegreeHomogeneityFast[f_,xxx__]:=Module[{vartmp,numvartmp,ftmp},{vartmp=Complement[Variables[f],xxx]; numvartmp=Table[vartmp[[i]]->RandomPrime[{1,1000}],{i,Length[vartmp]}];ftmp=f/.numvartmp;Exponent[Simplify[(ftmp/.Table[xxtmp->xxtmp \[Lambda],{xxtmp,xxx}])/ftmp],\[Lambda]]}];


allMonoms[n_,deg_,x_]:=DeleteCases[Coefficient[(List@@Expand[(1+Total[Array[x,n]])^deg]/. j_Integer*monom:_:>monom)/.{x[i_]:>\[Lambda] x[i]},\[Lambda]^deg],0]

homPols[n_,deg_,sym_]:=Module[{mons},
mons=If[deg==0,1,allMonoms[n,deg,x]];
Return[If[deg==0,sym,(Plus@@(sym/@((Exponent[#,Array[x,n]]&/@mons))mons ))/.{sym[A__]:>sym[Sequence@@A]}]];
]


OmegaCoefficient[dens__,kin__,\[Delta]_]:=Module[{u,f,n,loops},
u=UFdata[dens][[1]]/.{z->x};
f=UFdata[dens][[2]]/.{z->x}//.kin;
n=Length[dens[[1]]];
loops=Length[dens[[3]]];
PowerExpand[u^(n-\[Delta] loops-\[Delta])/f^(n-\[Delta] loops)(u^(loops+1)/f^loops)^eps]
]

(*Computes the numerators of derivatives of Omegas*)
DsOmega[dens__,kin__,invariants__,\[Delta]_,specialValues__]:=Module[{u,f,loops,omegaeps,ders},
omegaeps=OmegaCoefficient[dens,kin,\[Delta]];
ders=(Append[#,1]&)/@(List/@invariants);
Return[((D[omegaeps,Sequence@@ders]/omegaeps)//.specialValues)//Simplify//Numerator]
]

(*We organise derivatives by the maximum degree*)
AllDsOmega[dens__,kin__,invariants__,maxDegree_,\[Delta]_,specialValues__:{}]:=Reverse[Table[DsOmega[dens,kin,invariants[[1;;i]],\[Delta],specialValues],{i,1,maxDegree}]]

getEquations[pol_,vars__]:=(CoefficientArrays[{pol},vars]["NonzeroValues"])//Through//Flatten;

preparationData[dens__,kin__,myvars___:{Nothing}]:=Module[{u,f,nu,loops,vars,jacF,jacU,n,varslong},
u=UFdata[dens][[1]]/.{z->x};
f=UFdata[dens][[2]]/.{z->x}//.kin;
loops=Length[dens[[3]]];
vars=Variables[u];
jacF=JacIdeal[f,vars];
jacU=JacIdeal[u,vars];
n=Length[dens[[1]]];
varslong=Join[Variables[f],{eps},myvars];
{u,f,loops,vars,jacF,jacU,n,varslong}
]


initialReduction[dataIntegral__,degree_,pol_,labelStep_,\[Delta]_]:=Module[{degforF,hatCAnsatz,degforU,Qansatz,eqReductionF,eqReductionU,systemF,systemU,alleqs,allvars,mysolutions,Csol,Qsol,M,u,f,loops,vars,jacF,jacU,n,varslong,degpol},
{u,f,loops,vars,jacF,jacU,n,varslong}=dataIntegral;

degpol=DegreeHomogeneityFast[pol,vars][[1]];
degforF=degpol-loops;
(*degforF=DeleteDuplicates[(DegreeHomogeneityFast[pol,vars]-DegreeHomogeneityFast[jacF,vars])//Flatten][[1]];
*)
hatCAnsatz=Table[homPols[n,degforF,\[Lambda][labelStep,i]],{i,1,Length[vars]}];
(*degforU=DeleteDuplicates[(DegreeHomogeneityFast[hatCAnsatz.jacU,vars]-DegreeHomogeneityFast[u,vars])//Flatten][[1]];
*)
degforU=degpol-loops-1;
Qansatz=homPols[n,degforU,Q[labelStep]];
eqReductionF=(pol-hatCAnsatz . jacF);
eqReductionU=(hatCAnsatz . jacU-Qansatz u);
systemF=getEquations[eqReductionF,vars];
systemU=getEquations[eqReductionU,vars];
alleqs=Join[systemF,systemU];
allvars=Complement[Variables[alleqs],varslong];


Print["Solving a system of "<>ToString[Length[alleqs]]<>" equations and "<>ToString[Length[allvars]]<>" variables" ];
mysolutions=solsFF[Equal[#,0]&/@alleqs,allvars];
Csol=hatCAnsatz/.mysolutions;
Qsol=Qansatz/.mysolutions;

M=((n-(loops+1)(\[Delta]-eps)) Qsol+Sum[D[Csol[[i]],x[i]],{i,n}])/(degree+n-1+loops(eps-\[Delta]))//Expand;
Return[{mysolutions,Qsol,Csol,M}]
]

genReduction[dataIntegral__,degree_,M_,numOmega_,labelStep_,\[Delta]_]:=Module[{degforF,hatCAnsatz,degforU,Qansatz,eqReductionF,eqReductionU,systemF,systemU,alleqs,allvars,mysolutions,Csol,Qsol,Mnext,u,f,loops,vars,jacF,jacU,n,varslong,csol,pol,degpol},
{u,f,loops,vars,jacF,jacU,n,varslong}=dataIntegral;
pol=M+c numOmega;

degpol=DegreeHomogeneityFast[pol,vars][[1]];
degforF=degpol-loops;

(*degforF=DeleteDuplicates[(DegreeHomogeneityFast[numOmega,vars]-DegreeHomogeneityFast[jacF,vars])//Flatten][[1]];*)
hatCAnsatz=Table[homPols[n,degforF,\[Lambda][labelStep,i]],{i,1,Length[vars]}];
degforU=degpol-loops-1;
(*DeleteDuplicates[(DegreeHomogeneityFast[hatCAnsatz.jacU,vars]-DegreeHomogeneityFast[u,vars])//Flatten][[1]];*)
Qansatz=homPols[n,degforU,Q[labelStep]];
eqReductionF=(pol-hatCAnsatz . jacF);
eqReductionU=(hatCAnsatz . jacU-Qansatz u);
systemF=getEquations[eqReductionF,vars];
(*DeleteCases[Flatten[CoefficientList[eqReductionF,vars]],0];*)
systemU=getEquations[eqReductionU,vars];
(*DeleteCases[Flatten[CoefficientList[eqReductionU,vars]],0];*)

alleqs=Join[systemF,systemU];
allvars=Complement[Variables[alleqs],varslong];
Print["Solving a system of "<>ToString[Length[alleqs]]<>" equations and "<>ToString[Length[allvars]]<>" variables" ];
mysolutions=solsFF[Equal[#,0]&/@alleqs,allvars];
Csol=hatCAnsatz/.mysolutions;
Qsol=Qansatz/.mysolutions;
csol=c/.mysolutions;
Mnext=((n-(loops+1)(\[Delta]-eps)) Qsol+Sum[D[Csol[[i]],x[i]],{i,n}])/(degree+n-1+loops(eps-\[Delta]))//Expand;
Return[{mysolutions,Csol,Qsol,csol,Mnext}]
]


rls[X__,sols__]:=Fold[#1/.#2&,X,Table[sols[[i]][[1]],{i,1,Length[sols]}]]


coeffReduction[dataIntegral__,myOmegas__,Startdegree__,\[Delta]_]:=Module[{step1,myM,st,allSteps,rescs},
Print["Start of the reduction: step 1"];
step1=initialReduction[dataIntegral,Startdegree,myOmegas[[1]],Startdegree,\[Delta]];
myM[Startdegree]=step1[[-1]];
allSteps={};
Do[Print["Step "<>ToString[i]];
st[Startdegree-i+1]=genReduction[dataIntegral,Startdegree+1-i,myM[Startdegree+2-i],myOmegas[[i]],Startdegree+1-i,\[Delta]];
myM[Startdegree-i+1]=st[Startdegree-i+1][[-1]];
allSteps=Append[allSteps,st[Startdegree-i+1]];
,
{i,2,Startdegree,1}];
rescs=rls[Prepend[Reverse[Table[allSteps[[i]][[-2]],{i,1,Startdegree-1}]],-myM[1]],allSteps];
Return[rescs]
]

vanishFreeCoeffs[res__,dataIntegral__]:=Module[{freeVarsArray,arrayCoeffs,valsFreeCoeffs},
freeVarsArray=Complement[res//Variables,dataIntegral[[-1]]];
arrayCoeffs=CoefficientArrays[res[[#]],freeVarsArray]&/@Range[Length[res]];
valsFreeCoeffs=If[Length[arrayCoeffs[[#]]]>1,DeleteDuplicates[(arrayCoeffs[[#]][[2]]//Normal)/.{X->Prime[25],eps->1/Prime[56]}//Simplify],0]&/@Range[Length[res]];
Return[{arrayCoeffs[[#]][[1]]&/@Range[Length[res]],valsFreeCoeffs//Flatten}]
]


