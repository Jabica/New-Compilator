; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

declare void @prints(ptr)

declare i32 @copy(ptr, ptr, i32)

declare i32 @fill(ptr, i32, i32)

define i32 @fib(i32 %n) {
entry:
  %t = alloca i32, align 4
  %i = alloca i32, align 4
  %b = alloca i32, align 4
  %a = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  %n.val = load i32, ptr %n1, align 4
  %cmptmp = icmp sle i32 %n.val, 1
  %bool2i32 = zext i1 %cmptmp to i32
  %tobool = icmp ne i32 %bool2i32, 0
  br i1 %tobool, label %if.then, label %if.merge, !prof !0

if.then:                                          ; preds = %entry
  %n.val2 = load i32, ptr %n1, align 4
  ret i32 %n.val2

if.merge:                                         ; preds = %entry
  store i32 0, ptr %a, align 4
  store i32 1, ptr %b, align 4
  store i32 2, ptr %i, align 4
  br label %while.cond

while.cond:                                       ; preds = %while.body, %if.merge
  %i.val = load i32, ptr %i, align 4
  %n.val3 = load i32, ptr %n1, align 4
  %cmptmp4 = icmp sle i32 %i.val, %n.val3
  %bool2i325 = zext i1 %cmptmp4 to i32
  %tobool6 = icmp ne i32 %bool2i325, 0
  br i1 %tobool6, label %while.body, label %while.end, !prof !0

while.body:                                       ; preds = %while.cond
  %a.val = load i32, ptr %a, align 4
  %b.val = load i32, ptr %b, align 4
  %addtmp = add i32 %a.val, %b.val
  store i32 %addtmp, ptr %t, align 4
  %b.val7 = load i32, ptr %b, align 4
  store i32 %b.val7, ptr %a, align 4
  %t.val = load i32, ptr %t, align 4
  store i32 %t.val, ptr %b, align 4
  %i.val8 = load i32, ptr %i, align 4
  %addtmp9 = add i32 %i.val8, 1
  store i32 %addtmp9, ptr %i, align 4
  br label %while.cond

while.end:                                        ; preds = %while.cond
  %b.val10 = load i32, ptr %b, align 4
  ret i32 %b.val10
}

define i32 @main() {
entry:
  %r = alloca i32, align 4
  %fib.call = call i32 @fib(i32 10)
  store i32 %fib.call, ptr %r, align 4
  %r.val = load i32, ptr %r, align 4
  call void @printi(i32 %r.val)
  %r.val1 = load i32, ptr %r, align 4
  ret i32 %r.val1
}

!0 = !{!"branch_weights", i32 8, i32 8}
