;; extends

; Attribute/member access on any reference chain (var.foo, count.index,
; each.key, data.aws_ami.id, ...)
(get_attr
  (identifier) @variable.member)

; Terraform block keywords
((variable_expr
   (identifier) @variable.builtin)
 (#any-of? @variable.builtin "var" "local" "module" "data" "path" "terraform"))
