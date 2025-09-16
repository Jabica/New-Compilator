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
  store i32 0, ptr %i, align 4
  br label %while.cond

while.cond:                                       ; preds = %if.merge, %entry
  %i.val = load i32, ptr %i, align 4
  %cmptmp = icmp slt i32 %i.val, 5
  %bool2i32 = zext i1 %cmptmp to i32
  %tobool = icmp ne i32 %bool2i32, 0
  br i1 %tobool, label %while.body, label %while.end, !prof !0

while.body:                                       ; preds = %while.cond
  %i.val1 = load i32, ptr %i, align 4
  %cmptmp2 = icmp eq i32 %i.val1, 3
  %bool2i323 = zext i1 %cmptmp2 to i32
  %tobool4 = icmp ne i32 %bool2i323, 0
  br i1 %tobool4, label %if.then, label %if.merge, !prof !0

while.end:                                        ; preds = %while.cond
  ret i32 0

if.then:                                          ; preds = %while.body
  ret i32 99

if.merge:                                         ; preds = %while.body
  %i.val5 = load i32, ptr %i, align 4
  %addtmp = add i32 %i.val5, 1
  store i32 %addtmp, ptr %i, align 4
  br label %while.cond
}

!0 = !{!"branch_weights", i32 8, i32 8}
