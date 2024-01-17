(* ::Package:: *)

results=Import["/home/delacruz/double-box-GD-DATA.m"];
{C1sol,sols1,C2sol,sols2,omegaeps,coeffs}=results;

(*Symanzik polynomials*)
f=((-t  x[3] x[5] x[7]-s((x[1]+x[2]+x[3])x[4]x[6]+(x[4]+x[5]+x[6])x[1]x[2]+(x[1]+x[4])(x[2]+x[6])x[7]))/.{s->1,t->X})//Simplify;
u=x[3] x[4]+x[3] x[5]+x[3] x[6]+x[3] x[7]+x[4] x[7]+x[5] x[7]+x[6] x[7]+x[1] (x[4]+x[5]+x[6]+x[7])+x[2] (x[4]+x[5]+x[6]+x[7])//Simplify;
(*D=2+4eps*)
del=2;
loops=2;
n=7;
varslong=Join[Variables[f],{eps}];

(*Inhomogeneous terms: this might take a while*)

Bv=((((C2sol/(f*(1+n-loops*(del-eps)))+C1sol/(n-loops*(del-eps))))))//.Dispatch[sols1]//.Dispatch[sols2];
gradBv=(Plus@@Table[D[omegaeps*Bv[[i]],x[i]],{i,1,n}])//Expand;


(*check that gradient is independent of free variables*)
varsfreeB=Complement[Bv//Variables,varslong];
gradBVarray=CoefficientArrays[gradBv,varsfreeB];

Length[varsfreeB]



numvarlong=Table[xxx->RandomPrime[{1,200}],{xxx,varslong}];
sumchecknum=gradBVarray[[2]]/.numvarlong;
sumchecknum//Expand



DeleteDuplicates[(Expand[(gradBVarray[[2]][[#]])]//Simplify)&/@Range[Length[varsfreeB]]]



(*Final check*)
isZero=Plus@@{D[omegaeps,{X,2}],((2+eps+3 X)/(X+X^2))D[omegaeps,{X,1}],(-eps-2 eps^2+X)/(X^2 (1+X))}+gradBVarray[[1]];



numvarlong=Table[xxx->RandomPrime[{1,200}],{xxx,varslong}];
sumchecknum=isZero/.numvarlong;
sumchecknum//Expand
