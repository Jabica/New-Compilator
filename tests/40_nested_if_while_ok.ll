; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

declare void @prints(ptr)

declare i32 @copy(ptr, ptr, i32)

declare i32 @fill(ptr, i32, i32)

define i32 @main() {
entry:
  %i = alloca i32, align 4
  %s = alloca i32, align 4
  store i32 0, ptr %s, align 4
  store i32 0, ptr %i, align 4
  br label %while.cond

while.cond:                                       ; preds = %if.merge, %entry
  %i.val = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i.val, 4
  %bool2i32 = zext i1 %cmptmp to i32
  %tobool = icmp ne i32 %bool2i32, 0
  br i1 %tobool, label %while.body, label %while.end, !prof !0

while.body:                                       ; preds = %while.cond
  %i.val1 = load i32, ptr %i, align 4
  %modtmp = srem i32 %i.val1, 2
  %cmptmp2 = icmp eq i32 %modtmp, 0
  %bool2i323 = zext i1 %cmptmp2 to i32
  %tobool4 = icmp ne i32 %bool2i323, 0
  br i1 %tobool4, label %if.then, label %if.else, !prof !0

while.end:                                        ; preds = %while.cond
  %s.val9 = load i32, ptr %s, align 4
  ret i32 %s.val9

if.then:                                          ; preds = %while.body
  %s.val = load i32, ptr %s, align 4
  %addtmp = add i32 %s.val, 5
  store i32 %addtmp, ptr %s, align 4
  br label %if.merge

if.else:                                          ; preds = %while.body
  %s.val5 = load i32, ptr %s, align 4
  %addtmp6 = add i32 %s.val5, 1
  store i32 %addtmp6, ptr %s, align 4
  br label %if.merge

if.merge:                                         ; preds = %if.else, %if.then
  %i.val7 = load i32, ptr %i, align 4
  %addtmp8 = add i32 %i.val7, 1
  store i32 %addtmp8, ptr %i, align 4
  br label %while.cond
}

!0 = !{!"branch_weights", i32 8, i32 8}
