<<FiniteFlow`
<<Routines-GD.m

UDoubleBoxNonPlanar=x[1]*x[2] + x[2]*x[3] + x[2]*x[4] + x[1]*x[5] + x[2]*x[5] + x[3]*x[5] + x[4]*x[5] + 
 x[1]*x[6] + x[2]*x[6] + x[3]*x[6] + x[4]*x[6] + x[1]*x[7] + x[3]*x[7] + x[4]*x[7] + 
 x[5]*x[7] + x[6]*x[7];
FDoubleBoxNonPlanar=-(X*x[2]*x[4]*x[6]) + x[2]*(x[3]*x[5] - x[4]*x[6]) + X*x[4]*x[5]*x[7] + 
 x[1]*(x[2]*x[3] + x[6]*x[7] + x[3]*(x[5] + x[6] + x[7]));
 var = Variables[UDoubleBoxNonPlanar]
 nedges = Length[var]
 loop = DegreeHomogeneity[UDoubleBoxNonPlanar, var]
 kinematics = Complement[Variables[FDoubleBoxNonPlanar], var]
 varlong = Join[kinematics, {\[Epsilon]}, var]
 Dim = 4 - 2 \[Epsilon]
 \[Omega] = nedges - loop/2*Dim
 IntDoubleBoxNonPlanar = UDoubleBoxNonPlanar^(nedges - (loop + 1)/2 Dim)/FDoubleBoxNonPlanar^(nedges - loop/2 Dim);
  DDoublebox[r_] := D[IntDoubleBoxNonPlanar, {X, r}]/IntDoubleBoxNonPlanar // Numerator // Simplify;
listderivative = Reverse[Table[DDoublebox[i], {i, loop}]];
deglist = DegreeHomogeneity[listderivative, var]

Print["Step 1"];
solFstep1 = ReductionF[listderivative[[1]], deglist[[1]], 0, 0, Subscript[\[Lambda], 1], FDoubleBoxNonPlanar, var, varlong];
DeleteFile["DoubleBox-NonPlanar-redF-Step1.txt"];
Save["DoubleBox-NonPlanar-redF-Step1.txt",solFstep1];
solUstep1 = ReductionU[solFstep1[[1]], solFstep1[[2]], Subscript[Q, 1], UDoubleBoxNonPlanar, var, varlong];
DeleteFile["DoubleBox-NonPlanar-redU-Step1.txt"];
Save["DoubleBox-NonPlanar-redU-Step1.txt",solUstep1];
Print["Compute M1"];
M1=((\[Omega] - Dim/2)*solUstep1[[3]] + Sum[D[solUstep1[[1]][[i]], var[[i]]], {i, Length[var]}])/(2 - 1 + \[Omega]);
DeleteFile["DoubleBox-NonPlanar-M1.txt"];
Save["DoubleBox-NonPlanar-M1.txt",M1];

Print["Step 2"];
solFstep2 = ReductionF[M1,solUstep1[[2]],listderivative[[2]],Subscript[c,1] , Subscript[\[Lambda], 2], FDoubleBoxNonPlanar, var, varlong];
DeleteFile["DoubleBox-NonPlanar-redF-Step2.txt"];
Save["DoubleBox-NonPlanar-redF-Step2.txt",solFstep2];
solUstep2 = ReductionU[solFstep2[[1]],  solFstep2[[2]], Subscript[Q, 2], UDoubleBoxNonPlanar, var, varlong];
DeleteFile["DoubleBox-NonPlanar-redU-Step2.txt"];
Save["DoubleBox-NonPlanar-redU-Step2.txt",solUstep2];
Print["Compute M2"];
M2=((\[Omega] - Dim/2)*solUstep2[[3]] + Sum[D[solUstep2[[1]][[i]], var[[i]]], {i, Length[var]}])/(1 - 1 + \[Omega]);
DeleteFile["DoubleBox-NonPlanar-M2.txt"];
Save["DoubleBox-NonPlanar-M2.txt",M2];

