<< FiniteFlow`
<< "/Users/pierre/Git/TwistedGriffithsDwork/Mathematica/Routines-GD.m"
m1=1
m2=5
m3=11
F2sunset=-(t*x[1]*x[2]*x[3]) + (m1^2*x[1] + m2^2*x[2] + m3^2*x[3])*
  (x[1]*x[2] + x[1]*x[3] + x[2]*x[3]);
U2sunset=x[1]*x[2] + x[1]*x[3] + x[2]*x[3];
var = Variables[U2sunset];
powerU=3*\[Epsilon];
powerF=1 + 2*\[Epsilon];
Omega2sunset=U2sunset^powerU/F2sunset^powerF;
Kinematics = Complement[Variables[F2sunset], var];
varlong = Join[{\[Epsilon]}, Kinematics, var];
listderivative = 
 Reverse[Table[
   Simplify[D[Omega2sunset, {t, i}]/Omega2sunset] // Numerator, {i, 
    4}]];
listdegree = DegreeHomogeneity[listderivative, var];

derivativeorder = 4
RedFStep1 = 
  ReductionF[listderivative[[1]], listdegree[[1]], 0, 0, 
   Subscript[\[Lambda], 1], F2sunset, var, varlong];
RedUStep1 = 
  ReductionU[RedFStep1[[1]], RedFStep1[[2]], Subscript[Q, 1], 
   U2sunset, var, varlong];
Mstep1 = (Sum[D[RedUStep1[[1]][[i]], var[[i]]], {i, Length[var]}] + 
      powerU*RedUStep1[[3]] )/(derivativeorder - 1 + powerF) /. 
   RedUStep1[[4]];

derivativeorder = 3
RedFStep2 = 
  ReductionF[Mstep1, listdegree[[2]], listderivative[[2]], Subscript[
   c, 3], Subscript[\[Lambda], 2], F2sunset, var, varlong];
RedUStep2 = 
  ReductionU[RedFStep2[[1]], RedFStep2[[2]], Subscript[Q, 2], 
   U2sunset, var, varlong];
Mstep2 = (Sum[D[RedUStep2[[1]][[i]], var[[i]]], {i, Length[var]}] + 
      powerU*RedUStep2[[3]])/(derivativeorder - 1 + powerF) /. 
   RedUStep2[[4]];

derivativeorder = 2
RedFStep3 = 
  ReductionF[Mstep2, listdegree[[3]], listderivative[[3]], Subscript[
   c, 2], Subscript[\[Lambda], 3], F2sunset, var, varlong];
RedUStep3 = 
  ReductionU[RedFStep3[[1]], RedFStep3[[2]], Subscript[Q, 2], 
   U2sunset, var, varlong];
Mstep3 = (Sum[D[RedUStep3[[1]][[i]], var[[i]]], {i, Length[var]}] + 
      powerU*RedUStep3[[3]])/(derivativeorder - 1 + powerF) /. 
   RedUStep3[[4]];

derivativeorder = 1
RedFStep4 = 
  ReductionF[Mstep3, listdegree[[4]], listderivative[[4]], Subscript[
   c, 1], Subscript[\[Lambda], 3], F2sunset, var, varlong];
RedUStep4 = 
  ReductionU[RedFStep4[[1]], RedFStep4[[2]], Subscript[Q, 4], 
   U2sunset, var, varlong];
Mstep4 = (Sum[D[RedUStep4[[1]][[i]], var[[i]]], {i, Length[var]}] + 
      powerU*RedUStep4[[3]])/(derivativeorder - 1 + powerF) /. 
   RedUStep4[[4]];
filename="2sunset-3mass-"<>ToString[m1]<>"-"<>ToString[m2]<>"-"<>ToString[m3]<>".txt";
Print["Result of the differential operator saved in ",filename];
listPF = {-Mstep4, Subscript[c, 1], Subscript[c, 2], Subscript[c, 3], 
           1} //. RedFStep1[[4]] //. RedUStep1[[4]] //. 
        RedFStep2[[4]] //. RedUStep2[[4]] //. RedFStep3[[4]] //. 
     RedUStep3[[4]] //. RedFStep4[[4]] //. RedUStep4[[4]] ;
Save[filename,{listPF,Omega2sunset}];
