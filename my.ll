; ModuleID = 'my cool jit'

declare double @putchard(double)

declare double @putchars()

declare double @putchar(double)

define double @"unary!"(double %v) {
entry:
  %0 = fcmp one double 0.000000e+00, %v
  br i1 %0, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %1 = phi double [ 0.000000e+00, %if.then ], [ 1.000000e+00, %if.else ]
  ret double %1
}

define double @unary-(double %v) {
entry:
  %0 = fsub double 0.000000e+00, %v
  ret double %0
}

define double @"binary>"(double %LHS, double %RHS) {
entry:
  %0 = fcmp ult double %RHS, %LHS
  %1 = uitofp i1 %0 to double
  ret double %1
}

define double @"binary|"(double %LHS, double %RHS) {
entry:
  %0 = fcmp one double 0.000000e+00, %LHS
  br i1 %0, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  %1 = fcmp one double 0.000000e+00, %RHS
  br i1 %1, label %if.then1, label %if.else1

if.exit:                                          ; preds = %if.exit1, %if.then
  %2 = phi double [ 1.000000e+00, %if.then ], [ %3, %if.exit1 ]
  ret double %2

if.then1:                                         ; preds = %if.else
  br label %if.exit1

if.else1:                                         ; preds = %if.else
  br label %if.exit1

if.exit1:                                         ; preds = %if.else1, %if.then1
  %3 = phi double [ 1.000000e+00, %if.then1 ], [ 0.000000e+00, %if.else1 ]
  br label %if.exit
}

define double @"binary&"(double %LHS, double %RHS) {
entry:
  %0 = call double @"unary!"(double %LHS)
  %1 = fcmp one double 0.000000e+00, %0
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  %2 = call double @"unary!"(double %RHS)
  %3 = call double @"unary!"(double %2)
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %4 = phi double [ 0.000000e+00, %if.then ], [ %3, %if.else ]
  ret double %4
}

define double @"binary=="(double %LHS, double %RHS) {
entry:
  %0 = fcmp ult double %LHS, %RHS
  %1 = uitofp i1 %0 to double
  %2 = call double @"binary>"(double %LHS, double %RHS)
  %3 = call double @"binary|"(double %1, double %2)
  %4 = call double @"unary!"(double %3)
  ret double %4
}

define double @"binary:"(double %x, double %y) {
entry:
  ret double %y
}

define double @fibo(double %v) {
entry:
  %0 = call double @"binary=="(double %v, double 0.000000e+00)
  %1 = fcmp one double 0.000000e+00, %0
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  %2 = call double @"binary=="(double %v, double 1.000000e+00)
  %3 = fcmp one double 0.000000e+00, %2
  br i1 %3, label %if.then1, label %if.else1

if.exit:                                          ; preds = %if.exit1, %if.then
  %4 = phi double [ 0.000000e+00, %if.then ], [ %10, %if.exit1 ]
  ret double %4

if.then1:                                         ; preds = %if.else
  br label %if.exit1

if.else1:                                         ; preds = %if.else
  %5 = fsub double %v, 1.000000e+00
  %6 = call double @fibo(double %5)
  %7 = fsub double %v, 2.000000e+00
  %8 = call double @fibo(double %7)
  %9 = fadd double %6, %8
  br label %if.exit1

if.exit1:                                         ; preds = %if.else1, %if.then1
  %10 = phi double [ 1.000000e+00, %if.then1 ], [ %9, %if.else1 ]
  br label %if.exit
}

define double @fibi(double %x) {
entry:
  %0 = fadd double 1.000000e+00, 1.000000e+00
  %1 = call double @"binary:"(double %0, double %0)
  ret double %1
}

define double @wow() {
entry:
  %0 = alloca [1 x double]
  store [1 x double] [double 4.200000e+01], [1 x double]* %0
  %1 = call double @putchard(double 4.200000e+01)
  %2 = getelementptr inbounds [1 x double]* %0, double 0.000000e+00
  %3 = call double @putchard([1 x double]* %2)
  %4 = call double @"binary:"(double %1, double %3)
  ret double %4
}

define double @main() {
entry:
  %0 = call double @wow()
  ret double %0
}
