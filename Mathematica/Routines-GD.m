DegreeHomogeneityFast[f_,xxx__]:=Block[{vartmp,numvartmp,ftmp},vartmp=Complement[Variables[f],xxx]; numvartmp=Table[vartmp[[i]]->RandomPrime[{1,1000}],{i,Length[vartmp]}];ftmp=f/.numvartmp;Exponent[Simplify[(ftmp/.Table[xxtmp->xxtmp \[Lambda],{xxtmp,xxx}])/ftmp],\[Lambda]]];
DH[f_,var_]:=Max@Exponent[MonomialList[#]/.Thread[var->\[FormalX]],\[FormalX]]&[f];
numvar[loop_,edge_,derivative_]:= edge Binomial[(loop+1)derivative-loop+edge-1,(loop+1)derivative-loop]+Binomial[(loop+1)derivative-loop-1+edge,(loop+1)derivative-loop];
numeq[loop_,edge_,derivative_]:=  Binomial[edge+(loop+1)derivative-1,(loop+1)derivative]+Binomial[edge+(loop+1)(derivative-1)-1,(loop+1)(derivative-1)];
degP[loop_,derivative_]:=  (loop+1)*derivative;

JacIdeal[pol_, vars__] := D[pol, #] & /@ vars;
DegreeHomogeneity[f_, xxx__] := 
  Exponent[
   Simplify[(f /. Table[xxtmp -> xxtmp \[Lambda], {xxtmp, xxx}])/
     f], \[Lambda]];

degHom[pol_, var_] := 
 Exponent[
  FullSimplify[(pol /. 
      Table[xxxtmp -> \[Lambda] xxxtmp, {xxxtmp, var}])/
    pol], \[Lambda]];

allMonoms[n_, deg_, x_] := 
 DeleteCases[
  Coefficient[(List @@ Expand[(1 + Total[Array[x, n]])^deg] /. 
      j_Integer*monom : _ :> monom) /. {x[
       i_] :> \[Lambda] x[i]}, \[Lambda]^deg], 0]
homPols[n_, deg_, sym_] := Module[{mons},
  If[deg == 0, sym, mons = allMonoms[n, deg, x];
   Return[(Plus @@ (sym /@ ((Exponent[#, Array[x, n]] & /@ 
             mons)) mons )) /. {sym[A__] :> sym[Sequence @@ A]}];]
  ];

solsFF[coeffs__, vars__] := Module[{},
  FFDenseSolve[coeffs, vars, "ApplyFunction" -> Together, 
   MaxPrimes -> 10]
  ];

genEqsHatC[q_, f_, vars__] := Module[{deglamd, n, jacf},
  deglamd = degHom[q] - (degHom[f] - 1);
  jacf = JacIdeal[f, vars];
  n = Length[vars];
  Collect[ 
    q - Table[
       homPols[n, deglamd, \[Lambda][i]], {i, 1, Length[jacf]}] . 
      jacf, vars, Expand] // Expand
  ];

solsStepC[eqs_, vars__, varlong__] := Module[{arr1, varsEqs, sols},
  arr1 = 
   Flatten[CoefficientArrays[{eqs}, vars]["NonzeroValues"] // 
      Through] // Sort;
  varsEqs = Complement[Variables[eqs], varlong];
  sols = solsFF[Equal[#, 0] & /@ arr1, varsEqs]
  ];

solEquations[pol__, vars__, myVars__] := 
 Module[{arr1, varsEqs, sols},
  arr1 = 
   Flatten[CoefficientArrays[{pol}, vars]["NonzeroValues"] // 
      Through] // Sort;
  sols = solsFF[Equal[#, 0] & /@ arr1, myVars];
  Return[sols]
  ];

reductionJacobianF[poltoReduce_, pol_, vars__] := 
 Module[{jac, deg, degjac, Cs, toSolve, vareqs, varlong, SofEqs, sols},
  varlong = Join[Variables[pol], {\[Epsilon], t}];
  jac = JacIdeal[pol, vars];
  deg = degHom[poltoReduce, vars];
  degjac = degHom[jac[[1]], vars];
  Cs = Table[
    homPols[Length[vars], (deg - degjac), \[Lambda][j]], {j, 1, 
     Length[vars]}];
  toSolve = poltoReduce - Cs . jac; 
  SofEqs = DeleteCases[Flatten[CoefficientList[toSolve, vars]], 0];
  vareqs = Complement[Variables[toSolve], varlong];
  sols = solsFF[Equal[#, 0] & /@ SofEqs, vareqs] // Flatten;
  Return[{Cs /. sols, sols}]
  ];


getEquations[pol_,vars__]:=(CoefficientArrays[{pol},vars]["NonzeroValues"])//Through//Flatten;

ReductionF[M_,degreeP_, P_,coeffPtmp_,varunknown_,Fpol_,var_,varlong_]:=Block[{degtmp,poltoreduce,Chattmp,systmp,varsystmp,solfiniteflowtmp,Chattmpresult,coeffPresult,JacFtmp,Filenametmp},
Print["Reduction with respect to Jac(",Fpol,")"];JacFtmp=JacIdeal[Fpol,var];degtmp=degreeP-DegreeHomogeneity[JacFtmp,var][[1]];  Print["degree coefficient ",degtmp];Chattmp=Table[homPols[Length[var],degtmp,varunknown[r]],{r,Length[var]}];(*Print[Length[Chattmp]];*)
If[ListQ[P],poltoreduce=M+coeffPtmp.P-Chattmp.JacFtmp,poltoreduce=M+coeffPtmp*P-Chattmp.JacFtmp]; systmp=getEquations[poltoreduce,var];  varsystmp=Complement[Variables[systmp],varlong];  Print["Number of equations ",Length[systmp], ", number of variables ", Length[varsystmp]];
Filenametmp="Reduction-F-Case-"<>ToString[degreeP]<>"-"<>DateString[{"ISODate","-","Hour",":","Minute",":","Second"}]<>".txt";
Print["system saved in ",Filenametmp]; Save[Filenametmp,{systmp,varsystmp}];
Print["Calling Finite Flow "];solfiniteflowtmp=FFDenseSolve[Equal[#,0]&/@systmp,varsystmp,"ApplyFunction"->Together,MaxPrimes->20];
If[solfiniteflowtmp==FFImpossible, Echo["System cannot be solved"]];
Print["finite flow done -- length result ",Length[solfiniteflowtmp]]; Chattmpresult=Chattmp/.solfiniteflowtmp//Expand; coeffPresult=coeffPtmp/.solfiniteflowtmp//Expand;
{Chattmpresult,degtmp,coeffPresult,solfiniteflowtmp}]

ReductionU[M_,degree_,Qnametmp_,Upol_, var_,varlong_]:=Block[{degtmp,poltoreduce,Qtmp,systmp,varsystmp,solfiniteflowtmp,polU,Mresult,Qtmpresult,JacUtmp,Filenametmp},
Print["Reduction with respect to Jac(",Upol,")"]; JacUtmp=JacIdeal[Upol,var];polU=M.JacUtmp; degtmp=degree-1; Print["Degree quotient ",degtmp];Qtmp=homPols[Length[var],degtmp,Qnametmp];
poltoreduce=polU-Qtmp*Upol; systmp=DeleteCases[Flatten[CoefficientList[poltoreduce,var]],0];  varsystmp=Complement[Variables[systmp],varlong];  Print["Number of equations ",Length[systmp], ", number of variables ", Length[varsystmp]];
Filenametmp="Reduction-U-Case-"<>ToString[degree]<>"-"<>DateString[{"ISODate","-","Hour",":","Minute",":","Second"}]<>".txt";
Print["system saved in ",Filenametmp]; Save[Filenametmp,{systmp,varsystmp}];
Print["Calling Finite Flow "];solfiniteflowtmp=FFDenseSolve[Equal[#,0]&/@systmp,varsystmp,"ApplyFunction"->Together,MaxPrimes->20];
If[solfiniteflowtmp==FFImpossible, Echo["System cannot be solved"]];
Print["finite flow done -- length result ",Length[solfiniteflowtmp]];Mresult=M/.solfiniteflowtmp; Qtmpresult=Qtmp/.solfiniteflowtmp; {Mresult,degtmp,Qtmpresult,solfiniteflowtmp}]

Mroutine[Chat_, reducnum_, derivativeorder_, loop_,delta_,epsilonregulator_,var_] := (Sum[D[Chat[[i]], var[[i]]], {i, Length[var]}] + (Length[var] - (loop + 1)*(delta -epsilonregulator))* reducnum)/(derivativeorder - 1 +Length[var] - loop*(delta -epsilonregulator))

MroutineAnalytic[powerF_, powernumer_, Chat_, reducnum_, derivativeorder_, var_] := (Sum[D[Chat[[i]], var[[i]]], {i, Length[var]}] + powernumer*reducnum)/(derivativeorder - 1 +powerF)

Bvec[Chat_,derivativeorder_,Fpol_,powerF_]:=Chat/(Fpol^(derivativeorder-1))/(derivativeorder-1+powerF);
