module vast

import (
	v.token
	v.parser
	v.table
	v.ast
	os
	filepath
)

//generate json file with the same file name
pub fn json_file(file string) {
	ast_json:=json(file)

	filename:=filepath.filename(file)
	json_file:=filename[0..filename.len-2]+'.json'
	os.write_file(json_file,ast_json)
}

//generate json string
pub fn json(file string) string {
	if filepath.ext(file)≠'.v' {
		panic('the file must be v file')
	}

	apath:=abs_path(file)
	table:=table.Table{}
	ast_file:=parser.parse_file(apath,table,.parse_comments)

	//root of the tree
	root:=create_object()

	to_object(root,'path',string_node(ast_file.path))
	to_object(root,'mod',mod(ast_file.mod))
	to_object(root,'imports',imports(ast_file.imports))
	to_object(root,'scope',scope(ast_file.scope))
	to_object(root,'stmts',stmts(ast_file.stmts))
	//generate the ast string
	s:=json_print(root)
	return s
}

//basic type node
pub fn string_node(val string) &C.cJSON {
	return create_string(val)
}

pub fn number_node(val int) &C.cJSON {
	return create_number(val)
}

pub fn bool_node(val bool) &C.cJSON {
	if val {
		return create_true()
	} else {
		return create_false()
	}
}
pub fn null_node() &C.cJSON {
	return create_null()
}

//ast.File node
pub fn mod(mod ast.Module) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(mod.name))
	to_object(obj,'path',string_node(mod.path))
	to_object(obj,'expr',expr(mod.expr))
	return obj
}

pub fn imports(imports []ast.Import) &C.cJSON {
	imps:=create_array()
	for imp in imports {
		obj:=create_object()
		to_object(obj,'mod',string_node(imp.mod))
		to_object(obj,'alias',string_node(imp.alias))
		to_object(obj,'pos',position(imp.pos))
		to_array(imps,obj)
	}
	return imps
}

pub fn scope(scope ast.Scope) &C.cJSON {
	obj:=create_object()
	to_object(obj,'parent',string_node(ptr_str(scope.parent)))
	children_arr:=create_array()
	for s in scope.children {
		children_obj:=create_object()
		println(scope.parent)
		to_object(children_obj,'parent',string_node(ptr_str(s.parent)))
		to_object(children_obj,'start_pos',number_node(s.start_pos))
		to_object(children_obj,'end_pos',number_node(s.end_pos))
		to_array(children_arr,children_obj)
	}
	to_object(obj,'children',children_arr)
	to_object(obj,'start_pos',number_node(scope.start_pos))
	to_object(obj,'end_pos',number_node(scope.end_pos))
	return obj
}

//stmt node
pub fn stmts(stmts []ast.Stmt) &C.cJSON {
	stmt_array:=create_array()
	for s in stmts {
		to_array(stmt_array,stmt(s))
	}
	return stmt_array
}

pub fn stmt(node ast.Stmt) &C.cJSON {
	match node {
		ast.Module {
			return mod(it)
		}
		ast.Import {
			return import_(it)
		}
		ast.LineComment {
			return line_comment(it)
		}
		ast.MultiLineComment {
			return multi_line_comment(it)
		}
		ast.ConstDecl {
			return const_decl(it)
		}
		ast.FnDecl {
			return fn_decl(it)
		}
		ast.StructDecl {
			return struct_decl(it)
		}
		ast.EnumDecl {
			return enum_decl(it)
		}
		ast.Attr {
			return attr(it)
		}
		ast.HashStmt {
			return hash_stmt(it)
		}
		ast.CompIf {
			return comp_if(it)
		}
		ast.GlobalDecl {
			return global_decl(it)
		}
		ast.DeferStmt {
			return defer_stmt(it)
		}
		ast.TypeDecl {
			return type_decl(it)
		}
		ast.GotoLabel {
			return goto_label(it)
		}
		ast.GotoStmt {
			return goto_stmt(it)
		}
		// ast.Lambda {
		// 	return lambda(it)
		// }
		ast.AssignStmt {
			return assign_stmt(it)
		}
		ast.VarDecl {
			return var_decl(it)
		}
		ast.Return {
			return return_stmt(it)
		}
		ast.ForCStmt {
			return for_c_stmt(it)
		}
		ast.ForStmt {
			return for_stmt(it)
		}
		ast.ForInStmt {
			return for_in_stmt(it)
		}
		ast.BranchStmt {
			return branch_stmt(it)
		}
		ast.AssertStmt {
			return assert_stmt(it)
		}
		ast.UnsafeStmt {
			return unsafe_stmt(it)
		}
		ast.ExprStmt {
			return expr_stmt(it)
		}

		
		else {
			println('unknown node:$node')
			return string_node('unknown node')
		}
	}
}
pub fn import_(it ast.Import) &C.cJSON {
		obj:=create_object()
		to_object(obj,'ast_type',string_node('Import'))
		to_object(obj,'mod',string_node(it.mod))
		to_object(obj,'alias',string_node(it.alias))
		to_object(obj,'pos',position(it.pos))
		return obj
}
pub fn position(p token.Position) &C.cJSON {
	obj:=create_object()
	to_object(obj,'line_nr',number_node(p.line_nr))
	to_object(obj,'pos',number_node(p.pos))
	return obj
}

pub fn line_comment(it ast.LineComment) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('LineComment'))
	to_object(obj,'text',string_node(it.text))
	return obj
}
pub fn multi_line_comment(it ast.MultiLineComment) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('MultiLineComment'))
	to_object(obj,'text',string_node(it.text))
	return obj
}

pub fn const_decl(it ast.ConstDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('ConstDecl'))
	field_arr:=create_array()
	for f in it.fields {
		to_array(field_arr,field(f))
	}
	to_object(obj,'fields',field_arr)
	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'is_pub',bool_node(it.is_pub))

	return obj
}

pub fn fn_decl(it ast.FnDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('FnDecl'))
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'is_pub',bool_node(it.is_pub))
	to_object(obj,'is_variadic',bool_node(it.is_variadic))
	to_object(obj,'is_method',bool_node(it.is_method))
	to_object(obj,'rec_mut',bool_node(it.rec_mut))
	to_object(obj,'receiver',field(it.receiver))
	arg_arr:=create_array()
	for a in it.args {
		to_array(arg_arr,arg(a))
	}
	to_object(obj,'args',arg_arr)
	
	to_object(obj,'typ',number_node(int(it.typ)))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	return obj
}

pub fn struct_decl(it ast.StructDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('StructDecl'))
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'is_pub',bool_node(it.is_pub))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'mut_pos',number_node(it.mut_pos))
	to_object(obj,'pub_pos',number_node(it.pub_pos))
	to_object(obj,'pub_mut_pos',number_node(it.pub_mut_pos))
	f_arr:=create_array()
	for f in it.fields {
		to_array(f_arr,field(f))
	}
	to_object(obj,'fields',f_arr)
	return obj
}

pub fn enum_decl(it ast.EnumDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('EnumDecl'))
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'is_pub',bool_node(it.is_pub))
	str_arr:=create_array()
	for v in it.vals {
		to_array(str_arr,string_node(v))
	}
	to_object(obj,'vals',str_arr)
	return obj
}

pub fn attr(it ast.Attr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('Attr'))
	to_object(obj,'name',string_node(it.name))
	return obj
}

pub fn hash_stmt(it ast.HashStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('HashStmt'))
	to_object(obj,'val',string_node(it.val))
	return obj
}

pub fn comp_if(it ast.CompIf) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('CompIf'))
	to_object(obj,'cond',expr(it.cond))
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	else_stmt_arr:=create_array()
	for s in it.else_stmts {
		to_array(else_stmt_arr,stmt(s))
	}
	to_object(else_stmt_arr,'else_stmts',else_stmt_arr)

	return obj
}

pub fn global_decl(it ast.GlobalDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('GlobalDecl'))
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'expr',expr(it.expr))
	to_object(obj,'typ',number_node(int(it.typ)))
	return obj
}

pub fn defer_stmt(it ast.DeferStmt) &C.cJSON {
	obj:=create_object()
	stmt_array:=create_array()
	for s in it.stmts {
		to_array(stmt_array,stmt(s))
	}
	to_object(obj,'stmts',stmt_array)
	return obj
}

pub fn type_decl(it ast.TypeDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'ast_type',string_node('TypeDecl'))
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'is_pub',bool_node(it.is_pub))
	return obj
}

pub fn field(it ast.Field) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'typ',number_node(int(it.typ)))
	return obj
}

pub fn arg(it ast.Arg) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'typ',number_node(int(it.typ)))
	return obj
}

pub fn goto_label(it ast.GotoLabel) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	return obj
}
pub fn goto_stmt(it ast.GotoStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	return obj
}
pub fn lambda(it ast.Lambda) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	return obj
}

pub fn assign_stmt(it ast.AssignStmt) &C.cJSON {
	obj:=create_object()
	i_arr:=create_array()
	for i in it.left {
		to_array(i_arr,ident(i))
	}
	to_object(obj,'left',i_arr)

	e_arr:=create_array()
	for e in it.right {
		to_array(e_arr,expr(e))
	}
	to_object(obj,'right',e_arr)

	to_object(obj,'op',number_node(int(it.op)))
	to_object(obj,'pos',position(it.pos))

	return obj
}
pub fn var_decl(it ast.VarDecl) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'name2',string_node(it.name2))
	to_object(obj,'expr',expr(it.expr))
	to_object(obj,'is_mut',bool_node(it.is_mut))
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'pos',position(it.pos))

	return obj
}
pub fn return_stmt(it ast.Return) &C.cJSON {
	obj:=create_object()

	e_arr:=create_array()
	for e in it.exprs {
		to_array(e_arr,expr(e))
	}
	to_object(obj,'exprs',e_arr)

	to_object(obj,'pos',position(it.pos))
	return obj
}
pub fn for_c_stmt(it ast.ForCStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'init',stmt(it.init))
	to_object(obj,'cond',expr(it.cond))
	to_object(obj,'inc',expr(it.inc))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	return obj
}
pub fn for_stmt(it ast.ForStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'cond',expr(it.cond))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',position(it.pos))
	to_object(obj,'is_inf',bool_node(it.is_inf))
	return obj
}
pub fn for_in_stmt(it ast.ForInStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'key_var',string_node(it.key_var))
	to_object(obj,'val_var',string_node(it.val_var))
	to_object(obj,'cond',expr(it.cond))
	to_object(obj,'is_range',bool_node(it.is_range))
	to_object(obj,'high',expr(it.high))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',position(it.pos))

	return obj
}
pub fn branch_stmt(it ast.BranchStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'tok',number_node(int(&it.tok)))
	return obj
}
pub fn assert_stmt(it ast.AssertStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'expr',expr(it.expr))
	return obj
}
pub fn unsafe_stmt(it ast.UnsafeStmt) &C.cJSON {
	obj:=create_object()
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	return obj
}
pub fn expr_stmt(it ast.ExprStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'expr',expr(it.expr))
	return obj
}
//expr 
pub fn expr(e ast.Expr) &C.cJSON {
	match e {
		ast.IntegerLiteral {
			return integer_literal(it)
		} 
		ast.FloatLiteral {
			return float_literal(it)
		}
		ast.StringLiteral {
			return string_literal(it)
		}
		ast.CharLiteral {
			return char_literal(it)
		}
		ast.BoolLiteral {
			return bool_literal(it)
		}
		ast.EnumVal {
			return enum_val(it)
		}
		ast.Assoc {
			return assoc(it)
		}
		ast.CastExpr {
			return cast_expr(it)
		}
		ast.AsCast {
			return as_cast(it)
		}
		ast.Type {
			return type_expr(it)
		}
		ast.SizeOf {
			return size_of(it)
		}
		ast.PrefixExpr {
			return prefix_expr(it)
		}
		ast.AssignExpr {
			return assign_expr(it)
		}
		ast.InfixExpr {
			return infix_expr(it)
		}
		ast.IndexExpr {
			return index_expr(it)
		}
		ast.PostfixExpr {
			return postfix_expr(it)
		}
		ast.SelectorExpr {
			return selector_expr(it)
		}
		ast.RangeExpr {
			return range_expr(it)
		}
		ast.IfExpr {
			return if_expr(it)
		}
		ast.Ident {
			return ident(it)
		}
		ast.CallExpr {
			return call_expr(it)
		}
		ast.MethodCallExpr {
			return method_call_expr(it)
		}
		ast.OrExpr {
			return or_expr(it)
		}
		ast.StructInit {
			return struct_init(it)
		}
		ast.ArrayInit {
			return array_init(it)
		}
		ast.MapInit {
			return map_init(it)
		}
		ast.None {
			return none_expr(it)
		}
		ast.ParExpr {
			return par_expr(it)
		}
		ast.IfGuardExpr {
			return if_guard_expr(it)
		}
		ast.MatchExpr {
			return match_expr(it)
		}
		ast.ConcatExpr {
			return concat_expr(it)
		}


		else {
			// println('unknown expr')
			return null_node()
		}
	}	
}

pub fn integer_literal(it ast.IntegerLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',number_node(it.val))
	return obj	
}
pub fn float_literal(it ast.FloatLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',string_node(it.val))
	return obj	
}
pub fn string_literal(it ast.StringLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',string_node(it.val))
	return obj	
}
pub fn char_literal(it ast.CharLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',string_node(it.val))
	return obj	
}
pub fn bool_literal(it ast.BoolLiteral) &C.cJSON {
	obj:=create_object()
	to_object(obj,'val',bool_node(it.val))
	return obj	
}
pub fn enum_val(it ast.EnumVal) &C.cJSON {
	obj:=create_object()
	to_object(obj,'enum_name',string_node(it.enum_name))
	to_object(obj,'val',string_node(it.val))
	to_object(obj,'pos',position(it.pos))
	return obj		
}
pub fn assoc(it ast.Assoc) &C.cJSON {
	obj:=create_object()
	to_object(obj,'var_name',string_node(it.var_name))
	s_arr:=create_array()
	for f in it.fields {
		to_array(s_arr,string_node(f))
	}
	to_object(obj,'fields',s_arr)
	e_arr:=create_array()
	for e in it.exprs {
		to_array(e_arr,expr(e))
	}
	to_object(obj,'exprs',e_arr)
	to_object(obj,'pos',position(it.pos))
	return obj
}
pub fn cast_expr(it ast.CastExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'expr',expr(it.expr))
	return obj
}
pub fn as_cast(it ast.AsCast) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',number_node(int(it.typ)))
	return obj
}
pub fn type_expr(it ast.Type) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',number_node(int(it.typ)))
	return obj
}
pub fn size_of(it ast.SizeOf) &C.cJSON {
	obj:=create_object()
	to_object(obj,'type_name',string_node(it.type_name))
	return obj
}
pub fn prefix_expr(it ast.PrefixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',number_node(int(it.op)))
	to_object(obj,'right',expr(it.right))
	return obj
}
pub fn assign_expr(it ast.AssignExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',number_node(int(it.op)))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'left',expr(it.left))
	to_object(obj,'val',expr(it.val))
	return obj	
}
pub fn infix_expr(it ast.InfixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',number_node(int(it.op)))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'left',expr(it.left))
	to_object(obj,'left_type',number_node(int(it.left_type)))
	to_object(obj,'right',expr(it.right))
	to_object(obj,'right_type',number_node(int(it.right_type)))
	return obj
}
pub fn index_expr(it ast.IndexExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'left',expr(it.left))
	to_object(obj,'index',expr(it.index))
	return obj	
}
pub fn postfix_expr(it ast.PostfixExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'op',number_node(int(it.op)))
	to_object(obj,'expr',expr(it.expr))
	to_object(obj,'pos',position(it.pos))
	return obj	
}
pub fn selector_expr(it ast.SelectorExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'expr',expr(it.expr))
	to_object(obj,'field',string_node(it.field))
	return obj	
}
pub fn range_expr(it ast.RangeExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'low',expr(it.low))
	to_object(obj,'high',expr(it.high))
	return obj	
}
pub fn if_expr(it ast.IfExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'tok_kind',number_node(int(it.tok_kind)))
	to_object(obj,'cond',expr(it.cond))

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	else_arr:=create_array()
	for s in it.else_stmts {
		to_array(else_arr,stmt(s))
	}
	to_object(obj,'else_stmts',else_arr)	

	to_object(obj,'left',expr(it.left))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'has_else',bool_node(it.has_else))
	return obj	
}
pub fn ident(it ast.Ident) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'value',string_node(it.value))
	to_object(obj,'is_c',bool_node(it.is_c))
	to_object(obj,'tok_kind',number_node(int(it.tok_kind)))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'kind',number_node(int(it.kind)))
	to_object(obj,'info',ident_info(it.info))
	return obj	
}
pub fn ident_info(info ast.IdentInfo) &C.cJSON {
	match info {
		ast.IdentVar {
			return ident_var(it)
		}
		ast.IdentFunc {
			return ident_func(it)
		}
		else {
			return string_node('unknown node')
		}
	}
}
pub fn ident_var(it ast.IdentVar) &C.cJSON {
	obj:=create_object()
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'is_mut',bool_node(it.is_mut))
	to_object(obj,'is_static',bool_node(it.is_static))
	return obj	
}
pub fn ident_func(it ast.IdentFunc) &C.cJSON {
	obj:=create_object()
	to_object(obj,'return_type',number_node(int(it.return_type)))
	return obj	
}

pub fn call_expr(it ast.CallExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'is_c',bool_node(it.is_c))
	expr_arr:=create_array()
	for e in it.args {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'args',expr_arr)

	b_arr:=create_array()
	for b in it.muts {
		to_array(b_arr,bool_node(b))
	}
	to_object(obj,'muts',b_arr)

	to_object(obj,'or_block',or_expr(it.or_block))

	return obj	
}
pub fn method_call_expr(it ast.MethodCallExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj,'name',string_node(it.name))
	to_object(obj,'pos',position(it.pos))
	to_object(obj,'expr',expr(it.expr))
	expr_arr:=create_array()
	for e in it.args {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'args',expr_arr)

	b_arr:=create_array()
	for b in it.muts {
		to_array(b_arr,bool_node(b))
	}
	to_object(obj,'muts',b_arr)

	to_object(obj,'or_block',or_expr(it.or_block))

	to_object(obj,'typ',number_node(int(it.typ)))
	return obj	
}
pub fn or_expr(it ast.OrExpr) &C.cJSON {
	obj:=create_object()	
	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)
	return obj	
}
pub fn struct_init(it ast.StructInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',number_node(int(it.typ)))

	s_arr:=create_array()
	for f in it.fields {
		to_array(s_arr,string_node(f))
	}
	to_object(obj,'fields',s_arr)

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'pos',position(it.pos))

	return obj	
}
pub fn array_init(it ast.ArrayInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',number_node(int(it.typ)))
	to_object(obj,'elem_type',number_node(int(it.elem_type)))

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	to_object(obj,'pos',position(it.pos))

	return obj	
}
pub fn map_init(it ast.MapInit) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'typ',number_node(int(it.typ)))
	
	k_arr:=create_array()
	for k in it.keys {
		to_array(k_arr,expr(k))
	}
	to_object(obj,'keys',k_arr)

	v_arr:=create_array()
	for v in it.vals {
		to_array(v_arr,expr(v))
	}
	to_object(obj,'vals',v_arr)

	to_object(obj,'pos',position(it.pos))

	return obj	
}
pub fn none_expr(it ast.None) &C.cJSON {
	obj:=create_object()	
	//todo
	return obj	
}
pub fn par_expr(it ast.ParExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'expr',expr(it.expr))
	return obj	
}
pub fn if_guard_expr(it ast.IfGuardExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'var_name',string_node(it.var_name))
	to_object(obj,'expr',expr(it.expr))
	return obj	
}
pub fn match_expr(it ast.MatchExpr) &C.cJSON {
	obj:=create_object()	
	to_object(obj,'tok_kind',number_node(int(it.tok_kind)))
	to_object(obj,'cond',expr(it.cond))

	m_arr:=create_array()
	for b in it.branches {
		to_array(m_arr,match_branch(b))
	}
	to_object(obj,'branches',m_arr)
	to_object(obj,'expr_type',number_node(int(it.expr_type)))
	to_object(obj,'pos',position(it.pos))

	return obj	
}
pub fn match_branch(it ast.MatchBranch) &C.cJSON {
	obj:=create_object()

	expr_arr:=create_array()
	for e in it.exprs {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'exprs',expr_arr)

	stmt_arr:=create_array()
	for s in it.stmts {
		to_array(stmt_arr,stmt(s))
	}
	to_object(obj,'stmts',stmt_arr)

	to_object(obj,'pos',position(it.pos))
	return obj	
}
pub fn concat_expr(it ast.ConcatExpr) &C.cJSON {
	obj:=create_object()

	expr_arr:=create_array()
	for e in it.vals {
		to_array(expr_arr,expr(e))
	}
	to_object(obj,'vals',expr_arr)
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
	if filepath.is_abs(path) {
		return path
	} else if path.starts_with(filepath.separator) {
		return filepath.join(os.getwd(),path[2..])
	} else {
		return filepath.join(os.getwd(),path)
	}
}