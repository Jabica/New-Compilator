; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

declare void @prints(ptr)

declare i32 @copy(ptr, ptr, i32)

declare i32 @fill(ptr, i32, i32)

define i32 @main() {
entry:
  %x = alloca i32, align 4
  store i32 1, ptr %x, align 4
  %x.val = load i32, ptr %x, align 4
  %cmptmp = icmp eq i32 %x.val, 1
  %bool2i32 = zext i1 %cmptmp to i32
  %tobool = icmp ne i32 %bool2i32, 0
  br i1 %tobool, label %if.then, label %if.else, !prof !0

if.then:                                          ; preds = %entry
  ret i32 10

if.else:                                          ; preds = %entry
  br label %if.merge

if.merge:                                         ; preds = %if.else
  ret i32 0
}

!0 = !{!"branch_weights", i32 8, i32 8}
