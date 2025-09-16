; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

declare void @prints(ptr)

declare i32 @copy(ptr, ptr, i32)

declare i32 @fill(ptr, i32, i32)

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 2, ptr %a, align 4
  %a.val = load i32, ptr %a, align 4
  %cmptmp = icmp sgt i32 %a.val, 0
  %bool2i32 = zext i1 %cmptmp to i32
  %tobool = icmp ne i32 %bool2i32, 0
  br i1 %tobool, label %if.then, label %if.else, !prof !0

if.then:                                          ; preds = %entry
  br label %if.merge

if.else:                                          ; preds = %entry
  br label %if.merge

if.merge:                                         ; preds = %if.else, %if.then
  ret i32 1
}

!0 = !{!"branch_weights", i32 8, i32 8}
