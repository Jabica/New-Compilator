; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

declare void @prints(ptr)

declare i32 @copy(ptr, ptr, i32)

declare i32 @fill(ptr, i32, i32)

define i32 @fatorial(i32 %n) { ; (* AST: resultado -> %r, n -> %n1, enquanto (n > 1) validado pela semantica *)
entry:
  %r = alloca i32, align 4 ; (* variavel resultado: inteiro *)
  %n1 = alloca i32, align 4 ; (* copia mutavel do argumento n *)
  store i32 %n, ptr %n1, align 4
  store i32 1, ptr %r, align 4
  br label %while.cond ; (* salto para o enquanto do AST, com continua/quebra permitidos *)

while.cond:                                       ; preds = %while.body, %entry
  %n.val = load i32, ptr %n1, align 4
  %cmptmp = icmp sgt i32 %n.val, 1 ; (* comparacao gera i1 conforme regra semantica *)
  %bool2i32 = zext i1 %cmptmp to i32 ; (* conversao bool->int permitida *)
  %tobool = icmp ne i32 %bool2i32, 0 ; (* inteiro->logico valido pois valor e garantido 0/1 *)
  br i1 %tobool, label %while.body, label %while.end, !prof !0

while.body:                                       ; preds = %while.cond
  %r.val = load i32, ptr %r, align 4 ; (* corpo do enquanto do AST *)
  %n.val2 = load i32, ptr %n1, align 4
  %multmp = mul i32 %r.val, %n.val2
  store i32 %multmp, ptr %r, align 4
  %n.val3 = load i32, ptr %n1, align 4
  %subtmp = sub i32 %n.val3, 1
  store i32 %subtmp, ptr %n1, align 4
  br label %while.cond

while.end:                                        ; preds = %while.cond
  %r.val4 = load i32, ptr %r, align 4
  ret i32 %r.val4
}

define i32 @main() {
entry:
  %r = alloca i32, align 4
  %v = alloca i32, align 4
  store i32 5, ptr %v, align 4
  %v.val = load i32, ptr %v, align 4
  %fatorial.call = call i32 @fatorial(i32 %v.val)
  store i32 %fatorial.call, ptr %r, align 4
  %r.val = load i32, ptr %r, align 4
  call void @printi(i32 %r.val)
  %r.val1 = load i32, ptr %r, align 4
  ret i32 %r.val1
}

!0 = !{!"branch_weights", i32 8, i32 8}
