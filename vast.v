module main

import (
	v.token
	v.parser
	v.table
	v.ast
	os
)

const (
	version = '0.0.1'
)

pub fn main() {
	if os.args.len != 2 {
		println("unknown args,Usage:vast demo.v")
		return
	}
	file := os.args[1]
	if os.ext(file) != '.v' {
		println('the file must be v file')
		return
	}
	if !os.exists(file) {
		println('the v file does not exist')
		return
	}
	apath := abs_path(file)
	json_file(apath)
}

pub struct Tree {
	root &C.cJSON //the root of tree
	table &table.Table
}

//generate json file with the same file name
pub fn json_file(file string) {
	ast_json := json(file)
	
	json_file := file[0..file.len-2]+'.json'
	os.write_file(json_file,ast_json)
}

//generate json string
pub fn json(file string) string {
	t:=Tree {
		root: create_object()
		table: &table.Table{}
	}
	ast_file := parser.parse_file(file,t.table,.parse_comments)

	to_object(t.root,'path',t.string_node(ast_file.path))
	to_object(t.root,'mod',t.mod(ast_file.mod))
	to_object(t.root,'imports',t.imports(ast_file.imports))
	to_object(t.root,'scope',t.scope(ast_file.scope))
	to_object(t.root,'stmts',t.stmts(ast_file.stmts))
	//generate the ast string
	s := json_print(t.root)
	return s
}

//basic type node
pub fn (t Tree) string_node(val string) &C.cJSON {
	return create_string(val)
}

pub fn (t Tree) number_node(val int) &C.cJSON {
	return create_number(val)
}

pub fn (t Tree) bool_node(val bool) &C.cJSON {
	if val {
		return create_true()
	} else {
		return create_false()
	}
}
pub fn (t Tree) null_node() &C.cJSON {
	return create_null()
}
//todo
pub fn (t Tree) typ_node(typ table.Type) &C.cJSON {
	if typ == 0 {
		return create_string('')
	} else {
		// type_str:=t.table.get_type_symbol(typ)
		type_str:=t.table.type_to_str(typ)
		println(type_str)
		return create_string(type_str)
	}
}

//ast.File node
pub fn (t Tree) mod(mod ast.Module) &C.cJSON {
	obj := create_object()
	to_object(obj,'name',t.string_node(mod.name))
	to_object(obj,'path',t.string_node(mod.path))
	to_object(obj,'expr',t.expr(mod.expr))
	return obj
}

pub fn (t Tree) imports(imports []ast.Import) &C.cJSON {
	imps := create_array()
	for imp in imports {
		obj := create_object()
		to_object(obj,'mod',t.string_node(imp.mod))
		to_object(obj,'alias',t.string_node(imp.alias))
		to_object(obj,'pos',t.position(imp.pos))
		to_array(imps,obj)
	}
	return imps
}

pub fn (t Tree) scope(scope ast.Scope) &C.cJSON {
	obj := create_object()
	to_object(obj,'parent',t.string_node(ptr_str(scope.parent)))
	children_arr := create_array()
	for s in scope.children {
		children_obj := create_object()
		to_object(children_obj,'parent',t.string_node(ptr_str(s.parent)))
		to_object(children_obj,'start_pos',t.number_node(s.start_pos))
		to_object(children_obj,'end_pos',t.number_node(s.end_pos))
		to_array(children_arr,children_obj)
	}
	to_object(obj,'children',children_arr)
	to_object(obj,'start_pos',t.number_node(scope.start_pos))
	to_object(obj,'end_pos',t.number_node(scope.end_pos))
	return obj
}

//stmt node
pub fn (t Tree) stmts(stmts []ast.Stmt) &C.cJSON {
	stmt_array := create_array()
	for s in stmts {
		to_array(stmt_array,t.stmt(s))
	}
	return stmt_array
}

pub fn (t Tree) stmt(node ast.Stmt) &C.cJSON {
	match node {
		ast.Module {
			return t.mod(it)
		}
		ast.Import {
			return t.import_(it)
		}
		ast.LineComment {
			return t.line_comment(it)
		}
		ast.MultiLineComment {
			return t.multi_line_comment(it)
		}
		ast.ConstDecl {
			return t.const_decl(it)
		}
		ast.FnDecl {
			return t.fn_decl(it)
		}
		ast.StructDecl {
			return t.struct_decl(it)
		}
		ast.EnumDecl {
			return t.enum_decl(it)
		}
		ast.Attr {
			return t.attr(it)
		}
		ast.HashStmt {
			return t.hash_stmt(it)
		}
		ast.CompIf {
			return t.comp_if(it)
		}
		ast.GlobalDecl {
			return t.global_decl(it)
		}
		ast.DeferStmt {
			return t.defer_stmt(it)
		}
		ast.TypeDecl {
			return t.type_decl(it)
		}
		ast.GotoLabel {
			return t.goto_label(it)
		}
		ast.GotoStmt {
			return t.goto_stmt(it)
		}
		// ast.Lambda {
		// 	return t.lambda(it)
		// }
		ast.AssignStmt {
			return t.assign_stmt(it)
		}
		// ast.Var {
		// 	return t.var_decl(it)
		// }
		ast.Return {
			return t.return_(it)
		}
		// ast.ReturnStmt {
		// 	return t.return_stmt(it)
		// }
		ast.ForCStmt {
			return t.for_c_stmt(it)
		}
		ast.ForStmt {
			return t.for_stmt(it)
		}
		ast.ForInStmt {
			return t.for_in_stmt(it)
		}
		ast.BranchStmt {
			return t.branch_stmt(it)
		}
		ast.AssertStmt {
			return t.assert_stmt(it)
		}
		ast.UnsafeStmt {
			return t.unsafe_stmt(it)
		}
		ast.ExprStmt {
			return t.expr_stmt(it)
		}
		ast.GoStmt {
			return t.go_stmt(it)
		}
		ast.Block {
			return t.block(it)
		}
		else {
			println('unknown node:$node')
			return t.string_node('unknown node')
		}
	}
}
pub fn (t Tree) import_(it ast.Import) &C.cJSON {
		obj := create_object()
		to_object(obj,'ast_type',t.string_node('Import'))
		to_object(obj,'mod',t.string_node(it.mod))
		to_object(obj,'alias',t.string_node(it.alias))
		to_object(obj,'pos',t.position(it.pos))
		return obj
}
pub fn (t Tree) position(p token.Position) &C.cJSON {
	obj := create_object()
	to_object(obj,'line_nr',t.number_node(p.line_nr))
	to_object(obj,'pos',t.number_node(p.pos))
	return obj
}

pub fn (t Tree) line_comment(it ast.LineComment) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('LineComment'))
	to_object(obj,'text',t.string_node(it.text))
	return obj
}
pub fn (t Tree) multi_line_comment(it ast.MultiLineComment) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('MultiLineComment'))
	to_object(obj,'text',t.string_node(it.text))
	return obj
}

pub fn (t Tree) const_decl(it ast.ConstDecl) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('ConstDecl'))
	field_arr:=create_array()
	for f in it.fields {
		to_array(field_arr,t.field(f))
	}
	to_object(obj,'fields',field_arr)
	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,t.expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'is_pub',t.bool_node(it.is_pub))

	return obj
}

pub fn (t Tree) fn_decl(it ast.FnDecl) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('FnDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))
	to_object(obj,'is_variadic',t.bool_node(it.is_variadic))
	to_object(obj,'is_method',t.bool_node(it.is_method))
	to_object(obj,'rec_mut',t.bool_node(it.rec_mut))
	to_object(obj,'receiver',t.field(it.receiver))
	arg_arr := create_array()
	for a in it.args {
		to_array(arg_arr,t.arg(a))
	}
	to_object(obj,'args',arg_arr)
	
	to_object(obj,'return_type',t.number_node(int(it.return_type)))

	stmt_arr := create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	to_object(obj,'pos',t.position(it.pos))

	return obj
}

pub fn (t Tree) struct_decl(it ast.StructDecl) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('StructDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'mut_pos',t.number_node(it.mut_pos))
	to_object(obj,'pub_pos',t.number_node(it.pub_pos))
	to_object(obj,'pub_mut_pos',t.number_node(it.pub_mut_pos))
	to_object(obj,'is_c',t.bool_node(it.is_c))
	f_arr := create_array()
	for f in it.fields {
		to_array(f_arr,t.field(f))
	}
	to_object(obj,'fields',f_arr)
	return obj
}

pub fn (t Tree) enum_decl(it ast.EnumDecl) &C.cJSON {
	obj := create_object()
	to_object(obj,'ast_type',t.string_node('EnumDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))
	str_arr := create_array()
	for v in it.vals {
		to_array(str_arr,t.string_node(v))
	}
	to_object(obj,'vals',str_arr)
	return obj
}

pub fn (t Tree) attr(it ast.Attr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('Attr'))
	to_object(obj,'name',t.string_node(it.name))
	return obj
}

pub fn (t Tree) hash_stmt(it ast.HashStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('HashStmt'))
	to_object(obj,'val',t.string_node(it.val))
	return obj
}

pub fn (t Tree) comp_if(it ast.CompIf) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('CompIf'))
	to_object(obj,'val',t.string_node(it.val))
	to_object(obj,'is_not',t.bool_node(it.is_not))
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	else_stmt_arr:=create_array()
	for s in it.else_stmts {
		to_array(else_stmt_arr,t.stmt(s))
	}
	to_object(else_stmt_arr,'else_stmts',else_stmt_arr)
	to_object(obj,'has_else',t.bool_node(it.has_else))
	to_object(obj,'pos',t.position(it.pos))

	return obj
}

pub fn (t Tree) global_decl(it ast.GlobalDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('GlobalDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj
}

pub fn (t Tree) defer_stmt(it ast.DeferStmt) &C.cJSON {
	obj:=create_object()
	stmt_array:=create_array()
	for s in it.stmts {
		to_array(stmt_array,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_array)
	return obj
}

pub fn (t Tree) type_decl(node ast.TypeDecl) &C.cJSON {
	match node {
		ast.AliasTypeDecl {
			return t.alias_type_decl(it)
		}
		ast.SumTypeDecl {
			return t.sum_type_decl(it)
		}
		ast.FnTypeDecl {
			return t.fn_type_decl(it)
		}
		else {
			return t.string_node('unknown node')
		}
	}
}
pub fn (t Tree) alias_type_decl(it ast.AliasTypeDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('AliasTypeDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))
	to_object(obj,'parent_type',t.number_node(int(it.parent_type)))

	return obj
}
pub fn (t Tree) sum_type_decl(it ast.SumTypeDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('SumTypeDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))

	t_arr:=create_array()
	for s in it.sub_types {
		to_array(t_arr,t.number_node(int(s)))
	}
	to_object(obj,'sub_types',t_arr)
	
	return obj
}
pub fn (t Tree) fn_type_decl(it ast.FnTypeDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',t.string_node('FnTypeDecl'))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_pub',t.bool_node(it.is_pub))
	to_object(obj,'typ',t.number_node(int(it.typ)))

	return obj
}

//todo
pub fn (t Tree) field(it ast.Field) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	// to_object(obj,'typ',t.typ_node(it.typ))
	return obj
}

pub fn (t Tree) arg(it table.Arg) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'is_mut',t.bool_node(it.is_mut))
	return obj
}

pub fn (t Tree) goto_label(it ast.GotoLabel) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	return obj
}
pub fn (t Tree) goto_stmt(it ast.GotoStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	return obj
}
pub fn (t Tree) lambda(it ast.Lambda) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	return obj
}

pub fn (t Tree) assign_stmt(it ast.AssignStmt) &C.cJSON {
	obj:=create_object()
	i_arr:=create_array()
	for i in it.left {
		to_array(i_arr,t.ident(i))
	}
	to_object(obj,'left',i_arr)

	e_arr:=create_array()
	for e in it.right {
		to_array(e_arr,t.expr(e))
	}
	to_object(obj,'right',e_arr)

	to_object(obj,'op',t.number_node(int(it.op)))
	to_object(obj,'pos',t.position(it.pos))

	lt_arr:=create_array()
	for s in it.left_types {
		to_array(lt_arr,t.number_node(int(s)))
	}
	to_object(obj,'left_types',lt_arr)

	rt_arr:=create_array()
	for s in it.left_types {
		to_array(rt_arr,t.number_node(int(s)))
	}
	to_object(obj,'right_types',rt_arr)

	return obj
}
pub fn (t Tree) var_decl(it ast.Var) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'is_mut',t.bool_node(it.is_mut))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'pos',t.position(it.pos))

	return obj
}
pub fn (t Tree) return_(it ast.Return) &C.cJSON {
	obj:=create_object()

	e_arr:=create_array()
	for e in it.exprs {
		to_array(e_arr,t.expr(e))
	}
	to_object(obj,'exprs',e_arr)

	to_object(obj,'pos',t.position(it.pos))

	t_arr:=create_array()
	for s in it.types {
		to_array(t_arr,t.number_node(int(s)))
	}
	to_object(obj,'types',t_arr)

	return obj
}
pub fn (t Tree) return_stmt(it ast.ReturnStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'tok_kind',t.number_node(int(it.tok_kind)))
	to_object(obj,'pos',t.position(it.pos))

	e_arr:=create_array()
	for e in it.results {
		to_array(e_arr,t.expr(e))
	}
	to_object(obj,'results',e_arr)

	return obj
}
pub fn (t Tree) for_c_stmt(it ast.ForCStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'init',t.stmt(it.init))
	to_object(obj,'cond',t.expr(it.cond))
	to_object(obj,'inc',t.expr(it.inc))
	to_object(obj,'pos',t.position(it.pos))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	return obj
}
pub fn (t Tree) for_stmt(it ast.ForStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'cond',t.expr(it.cond))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'is_inf',t.bool_node(it.is_inf))
	return obj
}
pub fn (t Tree) for_in_stmt(it ast.ForInStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'key_var',t.string_node(it.key_var))
	to_object(obj,'val_var',t.string_node(it.val_var))
	to_object(obj,'cond',t.expr(it.cond))
	to_object(obj,'is_range',t.bool_node(it.is_range))
	to_object(obj,'high',t.expr(it.high))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'key_type',t.number_node(int(it.key_type)))
	to_object(obj,'val_type',t.number_node(int(it.val_type)))
	to_object(obj,'cond_type',t.number_node(int(it.cond_type)))
	to_object(obj,'kind',t.number_node(int(it.kind)))

	return obj
}
pub fn (t Tree) branch_stmt(it ast.BranchStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'tok',t.number_node(int(&it.tok)))
	return obj
}
pub fn (t Tree) assert_stmt(it ast.AssertStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',t.expr(it.expr))
	return obj
}
pub fn (t Tree) unsafe_stmt(it ast.UnsafeStmt) &C.cJSON {
	obj:=create_object()
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	return obj
}
pub fn (t Tree) go_stmt(it ast.GoStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',t.expr(it.expr))
	return obj
}
pub fn (t Tree) block(it ast.Block) &C.cJSON {
	obj:=create_object()
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	return obj	
}
pub fn (t Tree) expr_stmt(it ast.ExprStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'expr',t.expr(it.expr))
	return obj
}
//expr 
pub fn (t Tree) expr(e ast.Expr) &C.cJSON {
	match e {
		ast.IntegerLiteral {
			return t.integer_literal(it)
		} 
		ast.FloatLiteral {
			return t.float_literal(it)
		}
		ast.StringLiteral {
			return t.string_literal(it)
		}
		ast.CharLiteral {
			return t.char_literal(it)
		}
		ast.BoolLiteral {
			return t.bool_literal(it)
		}
		ast.StringInterLiteral {
			return t.string_inter_literal(it)
		}
		ast.EnumVal {
			return t.enum_val(it)
		}
		ast.Assoc {
			return t.assoc(it)
		}
		ast.CastExpr {
			return t.cast_expr(it)
		}
		ast.AsCast {
			return t.as_cast(it)
		}
		ast.Type {
			return t.type_expr(it)
		}
		ast.SizeOf {
			return t.size_of(it)
		}
		ast.PrefixExpr {
			return t.prefix_expr(it)
		}
		ast.AssignExpr {
			return t.assign_expr(it)
		}
		ast.InfixExpr {
			return t.infix_expr(it)
		}
		ast.IndexExpr {
			return t.index_expr(it)
		}
		ast.PostfixExpr {
			return t.postfix_expr(it)
		}
		ast.SelectorExpr {
			return t.selector_expr(it)
		}
		ast.RangeExpr {
			return t.range_expr(it)
		}
		ast.IfExpr {
			return t.if_expr(it)
		}
		ast.Ident {
			return t.ident(it)
		}
		ast.CallExpr {
			return t.call_expr(it)
		}
		ast.MethodCallExpr {
			return t.method_call_expr(it)
		}
		ast.OrExpr {
			return t.or_expr(it)
		}
		ast.StructInit {
			return t.struct_init(it)
		}
		ast.ArrayInit {
			return t.array_init(it)
		}
		ast.MapInit {
			return t.map_init(it)
		}
		ast.None {
			return t.none_expr(it)
		}
		ast.ParExpr {
			return t.par_expr(it)
		}
		ast.IfGuardExpr {
			return t.if_guard_expr(it)
		}
		ast.MatchExpr {
			return t.match_expr(it)
		}
		ast.ConcatExpr {
			return t.concat_expr(it)
		}
		ast.TypeOf {
			return t.type_of(it)
		}
		else {
			// println('unknown expr')
			return t.null_node()
		}
	}	
}

pub fn (t Tree) integer_literal(it ast.IntegerLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',t.string_node(it.val))
	return obj	
}
pub fn (t Tree) float_literal(it ast.FloatLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',t.string_node(it.val))
	return obj	
}
pub fn (t Tree) string_literal(it ast.StringLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',t.string_node(it.val))
	to_object(obj,'is_raw',t.bool_node(it.is_raw))
	to_object(obj,'is_c',t.bool_node(it.is_c))
	return obj	
}
pub fn (t Tree) char_literal(it ast.CharLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',t.string_node(it.val))
	return obj	
}
pub fn (t Tree) bool_literal(it ast.BoolLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',t.bool_node(it.val))
	return obj	
}
pub fn (t Tree) string_inter_literal(it ast.StringInterLiteral) &C.cJSON {
	obj:=create_object()
	v_arr:=create_array()
	for v in it.vals {
		to_array(v_arr,t.string_node(v))
	}
	to_object(obj,'vals',v_arr)
	
	s_arr:=create_array()
	for s in it.expr_fmts {
		to_array(s_arr,t.string_node(s))
	}
	to_object(obj,'expr_fmts',s_arr)

	e_arr:=create_array()
	for e in it.exprs {
		to_array(e_arr,t.expr(e))
	}
	to_object(obj,'exprs',e_arr)

	et_arr:=create_array()
	for e in it.expr_types {
		to_array(et_arr,t.number_node(int(e)))
	}
	to_object(obj,'expr_types',et_arr)

	return obj	
}
pub fn (t Tree) enum_val(it ast.EnumVal) &C.cJSON {
	obj:=create_object()
	to_object(obj,'enum_name',t.string_node(it.enum_name))
	to_object(obj,'mod',t.string_node(it.mod))
	to_object(obj,'val',t.string_node(it.val))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj		
}
pub fn (t Tree) assoc(it ast.Assoc) &C.cJSON {
	obj:=create_object()
	to_object(obj,'var_name',t.string_node(it.var_name))
	s_arr:=create_array()
	for f in it.fields {
		to_array(s_arr,t.string_node(f))
	}
	to_object(obj,'fields',s_arr)
	e_arr:=create_array()
	for e in it.exprs {
		to_array(e_arr,t.expr(e))
	}
	to_object(obj,'exprs',e_arr)
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj
}
pub fn (t Tree) cast_expr(it ast.CastExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'arg',t.expr(it.arg))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'expr_type',t.number_node(int(it.expr_type)))
	to_object(obj,'has_arg',t.bool_node(it.has_arg))
	return obj
}
pub fn (t Tree) as_cast(it ast.AsCast) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'expr_type',t.number_node(int(it.expr_type)))
	to_object(obj,'pos',t.position(it.pos))
	return obj
}
pub fn (t Tree) type_expr(it ast.Type) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj
}
pub fn (t Tree) size_of(it ast.SizeOf) &C.cJSON {
	obj:=create_object()
	to_object(obj,'type_name',t.string_node(it.type_name))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj
}
pub fn (t Tree) prefix_expr(it ast.PrefixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',t.number_node(int(it.op)))
	to_object(obj,'right',t.expr(it.right))
	return obj
}
pub fn (t Tree) assign_expr(it ast.AssignExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',t.number_node(int(it.op)))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'left',t.expr(it.left))
	to_object(obj,'val',t.expr(it.val))
	to_object(obj,'left_type',t.number_node(int(it.left_type)))
	to_object(obj,'right_type',t.number_node(int(it.right_type)))
	return obj	
}
pub fn (t Tree) infix_expr(it ast.InfixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',t.number_node(int(it.op)))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'left',t.expr(it.left))
	to_object(obj,'left_type',t.number_node(int(it.left_type)))
	to_object(obj,'right',t.expr(it.right))
	to_object(obj,'right_type',t.number_node(int(it.right_type)))
	return obj
}
pub fn (t Tree) index_expr(it ast.IndexExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'left',t.expr(it.left))
	to_object(obj,'index',t.expr(it.index))
	return obj	
}
pub fn (t Tree) postfix_expr(it ast.PostfixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',t.number_node(int(it.op)))
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'pos',t.position(it.pos))
	return obj	
}
pub fn (t Tree) selector_expr(it ast.SelectorExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'field',t.string_node(it.field))
	return obj	
}
pub fn (t Tree) range_expr(it ast.RangeExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'low',t.expr(it.low))
	to_object(obj,'high',t.expr(it.high))
	return obj	
}
pub fn (t Tree) if_expr(it ast.IfExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'tok_kind',t.number_node(int(it.tok_kind)))
	branch_arr:=create_array()
	for b in it.branches {
		to_array(branch_arr,t.if_branch(b))
	}
	to_object(obj,'branches',branch_arr)	

	to_object(obj,'left',t.expr(it.left))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'has_else',t.bool_node(it.has_else))
	to_object(obj,'is_expr',t.bool_node(it.is_expr))
	return obj	
}
pub fn (t Tree) if_branch(it ast.IfBranch) &C.cJSON {
	obj:=create_object()
	to_object(obj,'cond',t.expr(it.cond))
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	to_object(obj,'pos',t.position(it.pos))
	return obj
}
pub fn (t Tree) ident(it ast.Ident) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'value',t.string_node(it.value))
	to_object(obj,'is_c',t.bool_node(it.is_c))
	to_object(obj,'tok_kind',t.number_node(int(it.tok_kind)))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'kind',t.number_node(int(it.kind)))
	to_object(obj,'info',t.ident_info(it.info))
	return obj	
}
pub fn (t Tree) ident_info(info ast.IdentInfo) &C.cJSON {
	match info {
		ast.IdentVar {
			return t.ident_var(it)
		}
		ast.IdentFn {
			return t.ident_fn(it)
		}
		else {
			return t.string_node('unknown node')
		}
	}
}
pub fn (t Tree) ident_var(it ast.IdentVar) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'is_mut',t.bool_node(it.is_mut))
	to_object(obj,'is_static',t.bool_node(it.is_static))
	to_object(obj,'is_optional',t.bool_node(it.is_optional))
	return obj	
}
pub fn (t Tree) ident_fn(it ast.IdentFn) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',t.number_node(int(it.typ)))
	return obj	
}

pub fn (t Tree) call_expr(it ast.CallExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'is_c',t.bool_node(it.is_c))
	arg_arr:=create_array()
	for e in it.args {
		to_array(arg_arr,t.call_arg(e))
	}
	to_object(obj,'args',arg_arr)

	b_arr:=create_array()
	for b in it.muts {
		to_array(b_arr,t.bool_node(b))
	}
	to_object(obj,'muts',b_arr)

	to_object(obj,'or_block',t.or_expr(it.or_block))
	to_object(obj,'return_type',t.number_node(int(it.return_type)))

	return obj	
}
pub fn (t Tree) method_call_expr(it ast.MethodCallExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'name',t.string_node(it.name))
	to_object(obj,'expr',t.expr(it.expr))
	arg_arr:=create_array()
	for e in it.args {
		to_array(arg_arr,t.call_arg(e))
	}
	to_object(obj,'args',arg_arr)
	to_object(obj,'or_block',t.or_expr(it.or_block))
	to_object(obj,'expr_type',t.number_node(int(it.expr_type)))
	to_object(obj,'receiver_type',t.number_node(int(it.receiver_type)))
	to_object(obj,'return_type',t.number_node(int(it.return_type)))
	return obj	
}
pub fn (t Tree) call_arg(it ast.CallArg) &C.cJSON {
	obj:=create_object()
	to_object(obj,'is_mut',t.bool_node(it.is_mut))
	to_object(obj,'expr',t.expr(it.expr))
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'expected_type',t.number_node(int(it.expected_type)))
	return obj	
}
pub fn (t Tree) or_expr(it ast.OrExpr) &C.cJSON {
	obj:=create_object()	
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	return obj	
}
pub fn (t Tree) struct_init(it ast.StructInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',t.number_node(int(it.typ)))

	s_arr:=create_array()
	for f in it.fields {
		to_array(s_arr,t.string_node(f))
	}
	to_object(obj,'fields',s_arr)

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,t.expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'pos',t.position(it.pos))

	expr_types_arr:=create_array()
	for e in it.expr_types {
		to_array(expr_types_arr,t.number_node(int(e)))
	}
	to_object(obj,'expr_types',expr_types_arr)

	expected_types_arr:=create_array()
	for e in it.expected_types {
		to_array(expected_types_arr,t.number_node(int(e)))
	}
	to_object(obj,'expected_types',expected_types_arr)


	return obj	
}
pub fn (t Tree) array_init(it ast.ArrayInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',t.number_node(int(it.typ)))
	to_object(obj,'elem_type',t.number_node(int(it.elem_type)))

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,t.expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'pos',t.position(it.pos))

	return obj	
}
pub fn (t Tree) map_init(it ast.MapInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',t.number_node(int(it.typ)))
	
	k_arr:=create_array()
	for k in it.keys {
		to_array(k_arr,t.expr(k))
	}
	to_object(obj,'keys',k_arr)

	v_arr:=create_array()
	for v in it.vals {
		to_array(v_arr,t.expr(v))
	}
	to_object(obj,'vals',v_arr)

	to_object(obj,'pos',t.position(it.pos))

	return obj	
}
pub fn (t Tree) none_expr(it ast.None) &C.cJSON {
	obj:=create_object()	
	//todo
	to_object(obj,'foo',t.number_node(it.foo))
	return obj	
}
pub fn (t Tree) par_expr(it ast.ParExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'expr',t.expr(it.expr))
	return obj	
}
pub fn (t Tree) if_guard_expr(it ast.IfGuardExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'var_name',t.string_node(it.var_name))
	to_object(obj,'expr',t.expr(it.expr))
	return obj	
}
pub fn (t Tree) match_expr(it ast.MatchExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'tok_kind',t.number_node(int(it.tok_kind)))
	to_object(obj,'cond',t.expr(it.cond))

	m_arr:=create_array()
	for b in it.branches {
		to_array(m_arr,t.match_branch(b))
	}
	to_object(obj,'branches',m_arr)
	to_object(obj,'cond_type',t.number_node(int(it.cond_type)))
	to_object(obj,'return_type',t.number_node(int(it.return_type)))
	to_object(obj,'pos',t.position(it.pos))
	to_object(obj,'is_sum_type',t.bool_node(it.is_sum_type))
	to_object(obj,'is_expr',t.bool_node(it.is_expr))

	return obj	
}
pub fn (t Tree) match_branch(it ast.MatchBranch) &C.cJSON {
	obj:=create_object()

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,t.expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,t.stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',t.position(it.pos))
	return obj	
}
pub fn (t Tree) concat_expr(it ast.ConcatExpr) &C.cJSON {
	obj:=create_object()

	expr_arr:=create_array()
	for e in it.vals {
		to_array(expr_arr,t.expr(e))
	}
	to_object(obj,'vals',expr_arr)
	return obj	
}
pub fn (t Tree) type_of(it ast.TypeOf) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',t.expr(it.expr))
	return obj
}

[inline]
pub fn to_object(node &C.cJSON,key string,child &C.cJSON) {
	add_item_to_object(node,key,child)
}
[inline]
pub fn to_array(node &C.cJSON,child &C.cJSON) {
	add_item_to_array(node,child)
}

//get absolute path for file
pub fn abs_path(path string) string {
	if os.is_abs_path(path) {
		return path
	} else if path.starts_with(os.path_separator) {
		return os.join_path(os.getwd(),path[2..])
	} else {
		return os.join_path(os.getwd(),path)
	}
}
