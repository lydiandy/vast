module main

import v.token
import v.parser
import v.table
import v.ast
import v.pref
import v.errors
import os

const (
	version = '0.0.1'
)

fn main() {
	if os.args.len != 2 {
		println('unknown args,Usage:vast demo.v')
		return
	}
	file := os.args[1]
	// file:='./example/demo.v'
	if os.file_ext(file) != '.v' {
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

struct Tree {
	root         &C.cJSON // the root of tree
	table        &table.Table
	pref         pref.Preferences
	global_scope &ast.Scope
}

// generate json file with the same file name
fn json_file(file string) {
	ast_json := json(file)
	json_file := file[0..file.len - 2] + '.json'
	os.write_file(json_file, ast_json)
}

// generate json string
fn json(file string) string {
	t := Tree{
		root: create_object()
		table: table.new_table()
		pref: pref.new_preferences()
		global_scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
	}
	ast_file := parser.parse_file(file, t.table, .parse_comments, t.pref, t.global_scope)
	// ast_file := parser.parse_file(file, t.table, .skip_comments, t.pref, t.global_scope)
	to_object(t.root, 'ast_type', t.string_node('ast.File'))
	to_object(t.root, 'path', t.string_node(ast_file.path))
	to_object(t.root, 'mod', t.mod(ast_file.mod))
	to_object(t.root, 'imports', t.imports(ast_file.imports))
	to_object(t.root, 'scope', t.scope(ast_file.scope))
	to_object(t.root, 'global_scope', t.scope(ast_file.global_scope))
	to_object(t.root, 'errors', t.errors(ast_file.errors))
	to_object(t.root, 'warnings', t.warnings(ast_file.warnings))
	to_object(t.root, 'stmts', t.stmts(ast_file.stmts))
	// generate the ast string
	s := json_print(t.root)
	return s
}

// string type node
fn (t Tree) string_node(val string) &C.cJSON {
	return create_string(val)
}

// number type node
fn (t Tree) number_node(val int) &C.cJSON {
	return create_number(val)
}

// bool type node
fn (t Tree) bool_node(val bool) &C.cJSON {
	if val {
		return create_true()
	} else {
		return create_false()
	}
}

// null type node
fn (t Tree) null_node() &C.cJSON {
	return create_null()
}

// type type node
fn (t Tree) type_node(typ table.Type) &C.cJSON {
	if typ == 0 {
		return create_string('')
	} else {
		type_name := t.table.get_type_name(typ)
		return create_string(type_name)
	}
}

// todo:enum type node
fn (t Tree) enum_node(e int) &C.cJSON {
	return t.string_node('enum test')
}

// ast.File node
fn (t Tree) mod(mod ast.Module) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Module'))
	to_object(obj, 'name', t.string_node(mod.name))
	to_object(obj, 'path', t.string_node(mod.path))
	to_object(obj, 'expr', t.expr(mod.expr))
	to_object(obj, 'is_skipped', t.bool_node(mod.is_skipped))
	to_object(obj, 'pos', t.position(mod.pos))
	return obj
}

fn (t Tree) scope(scope ast.Scope) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Scope'))
	to_object(obj, 'parent', t.string_node(ptr_str(scope.parent)))
	children_arr := create_array()
	for s in scope.children {
		mut children_obj := create_object()
		to_object(children_obj, 'parent', t.string_node(ptr_str(s.parent)))
		to_object(children_obj, 'start_pos', t.number_node(s.start_pos))
		to_object(children_obj, 'end_pos', t.number_node(s.end_pos))
		to_array(children_arr, children_obj)
	}
	to_object(obj, 'children', children_arr)
	to_object(obj, 'start_pos', t.number_node(scope.start_pos))
	to_object(obj, 'end_pos', t.number_node(scope.end_pos))
	to_object(obj, 'objects', t.objects(scope.objects))
	return obj
}

fn (t Tree) objects(so map[string]ast.ScopeObject) &C.cJSON {
	obj := create_object()
	for key, val in so {
		to_object(obj, key, t.scope_object(val))
	}
	return obj
}

fn (t Tree) scope_object(node ast.ScopeObject) &C.cJSON {
	obj := create_object()
	match node {
		ast.ConstField { t.const_field(node) }
		ast.GlobalDecl { t.global_decl(node) }
		ast.Var { t.var(node) }
	}
	return obj
}

fn (t Tree) imports(imports []ast.Import) &C.cJSON {
	imps := create_array()
	for imp in imports {
		obj := create_object()
		to_object(obj, 'mod', t.string_node(imp.mod))
		to_object(obj, 'alias', t.string_node(imp.alias))
		to_object(obj, 'pos', t.position(imp.pos))
		to_array(imps, obj)
	}
	return imps
}

fn (t Tree) errors(errors []errors.Error) &C.cJSON {
	errs := create_array()
	for e in errors {
		obj := create_object()
		to_object(obj, 'message', t.string_node(e.message))
		to_object(obj, 'file_path', t.string_node(e.file_path))
		to_object(obj, 'pos', t.position(e.pos))
		to_object(obj, 'backtrace', t.string_node(e.backtrace))
		to_object(obj, 'reporter', t.number_node(int(e.reporter)))
		to_array(errs, obj)
	}
	return errs
}

fn (t Tree) warnings(warnings []errors.Warning) &C.cJSON {
	warns := create_array()
	for w in warnings {
		obj := create_object()
		to_object(obj, 'message', t.string_node(w.message))
		to_object(obj, 'file_path', t.string_node(w.file_path))
		to_object(obj, 'pos', t.position(w.pos))
		to_object(obj, 'reporter', t.number_node(int(w.reporter)))
		to_array(warns, obj)
	}
	return warns
}

fn (t Tree) var(node ast.Var) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Var'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'is_used', t.bool_node(node.is_used))
	return obj
}

// stmt node
fn (t Tree) stmts(stmts []ast.Stmt) &C.cJSON {
	stmt_array := create_array()
	for s in stmts {
		to_array(stmt_array, t.stmt(s))
	}
	return stmt_array
}

fn (t Tree) stmt(node ast.Stmt) &C.cJSON {
	match node {
		ast.Module { return t.mod(node) }
		ast.Import { return t.import_(node) }
		ast.Comment { return t.comment(node) }
		ast.ConstDecl { return t.const_decl(node) }
		ast.FnDecl { return t.fn_decl(node) }
		ast.StructDecl { return t.struct_decl(node) }
		ast.EnumDecl { return t.enum_decl(node) }
		ast.InterfaceDecl { return t.interface_decl(node) }
		ast.Attr { return t.attr(node) }
		ast.HashStmt { return t.hash_stmt(node) }
		ast.CompIf { return t.comp_if(node) }
		ast.GlobalDecl { return t.global_decl(node) }
		ast.DeferStmt { return t.defer_stmt(node) }
		ast.TypeDecl { return t.type_decl(node) }
		ast.GotoLabel { return t.goto_label(node) }
		ast.GotoStmt { return t.goto_stmt(node) }
		ast.AssignStmt { return t.assign_stmt(node) }
		ast.Return { return t.return_(node) }
		ast.ForCStmt { return t.for_c_stmt(node) }
		ast.ForStmt { return t.for_stmt(node) }
		ast.ForInStmt { return t.for_in_stmt(node) }
		ast.BranchStmt { return t.branch_stmt(node) }
		ast.AssertStmt { return t.assert_stmt(node) }
		ast.UnsafeStmt { return t.unsafe_stmt(node) }
		ast.ExprStmt { return t.expr_stmt(node) }
		ast.GoStmt { return t.go_stmt(node) }
		ast.Block { return t.block(node) }
		ast.ComptimeCall { return t.comptime_call(node) }
	}
}

fn (t Tree) import_(node ast.Import) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Import'))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'alias', t.string_node(node.alias))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) position(p token.Position) &C.cJSON {
	obj := create_object()
	to_object(obj, 'line_nr', t.number_node(p.line_nr))
	to_object(obj, 'pos', t.number_node(p.pos))
	to_object(obj, 'len', t.number_node(p.len))
	return obj
}

fn (t Tree) comment(node ast.Comment) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Comment'))
	to_object(obj, 'text', t.string_node(node.text))
	// to_object(obj, 'is_multi', t.bool_node(node.is_multi)) //maybe need remove from ast
	// to_object(obj, 'line_nr', t.number_node(node.line_nr)) //maybe need remove from ast
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_decl(node ast.ConstDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ConstDecl'))
	field_arr := create_array()
	for f in node.fields {
		to_array(field_arr, t.const_field(f))
	}
	to_object(obj, 'fields', field_arr)
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_field(node ast.ConstField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ConstField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'comment', t.comment(node.comment))
	return obj
}

fn (t Tree) fn_decl(node ast.FnDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('FnDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'is_deprecated', t.bool_node(node.is_deprecated))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_variadic', t.bool_node(node.is_variadic))
	to_object(obj, 'is_anon', t.bool_node(node.is_anon))
	to_object(obj, 'receiver', t.field(node.receiver))
	to_object(obj, 'receiver_pos', t.position(node.receiver_pos))
	to_object(obj, 'is_method', t.bool_node(node.is_method))
	to_object(obj, 'rec_mut', t.bool_node(node.rec_mut))
	// to_object(obj, 'language', t.enum_node(node.language))
	to_object(obj, 'language', t.number_node(int(node.language)))
	to_object(obj, 'no_body', t.bool_node(node.no_body))
	to_object(obj, 'is_builtin', t.bool_node(node.is_builtin))
	to_object(obj, 'is_generic', t.bool_node(node.is_generic))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'body_pos', t.position(node.body_pos))
	to_object(obj, 'file', t.string_node(node.file))
	arg_arr := create_array()
	for a in node.args {
		to_array(arg_arr, t.arg(a))
	}
	to_object(obj, 'args', arg_arr)
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	return obj
}

fn (t Tree) anon_fn(node ast.AnonFn) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AnonFn'))
	to_object(obj, 'decl', t.fn_decl(node.decl))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) struct_decl(node ast.StructDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'mut_pos', t.number_node(node.mut_pos))
	to_object(obj, 'pub_pos', t.number_node(node.pub_pos))
	to_object(obj, 'pub_mut_pos', t.number_node(node.pub_mut_pos))
	to_object(obj, 'language', t.number_node(int(node.language)))
	to_object(obj, 'is_union', t.bool_node(node.is_union))
	a_arr := create_array()
	for a in node.attrs {
		to_array(a_arr, t.string_node(a))
	}
	to_object(obj, 'attrs', a_arr)
	f_arr := create_array()
	for f in node.fields {
		to_array(f_arr, t.struct_field(f))
	}
	to_object(obj, 'fields', f_arr)
	return obj
}

fn (t Tree) enum_decl(node ast.EnumDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_flag', t.bool_node(node.is_flag))
	to_object(obj, 'pos', t.position(node.pos))
	f_arr := create_array()
	for f in node.fields {
		to_array(f_arr, t.enum_field(f))
	}
	to_object(obj, 'fields', f_arr)
	return obj
}

fn (t Tree) enum_field(node ast.EnumField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'has_expr', t.bool_node(node.has_expr))
	to_object(obj, 'expr', t.expr(node.expr))
	return obj
}

fn (t Tree) interface_decl(node ast.InterfaceDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('InterfaceDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	str_arr := create_array()
	for s in node.field_names {
		to_array(str_arr, t.string_node(s))
	}
	to_object(obj, 'field_names', str_arr)
	m_arr := create_array()
	for m in node.methods {
		to_array(m_arr, t.fn_decl(m))
	}
	to_object(obj, 'methods', m_arr)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) attr(node ast.Attr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Attr'))
	to_object(obj, 'name', t.string_node(node.name))
	return obj
}

fn (t Tree) hash_stmt(node ast.HashStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('HashStmt'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'mod', t.string_node(node.mod))
	return obj
}

fn (t Tree) comp_if(node ast.CompIf) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CompIf'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'is_not', t.bool_node(node.is_not))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	else_stmt_arr := create_array()
	for s in node.else_stmts {
		to_array(else_stmt_arr, t.stmt(s))
	}
	to_object(else_stmt_arr, 'else_stmts', else_stmt_arr)
	to_object(obj, 'has_else', t.bool_node(node.has_else))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) global_decl(node ast.GlobalDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GlobalDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'has_expr', t.bool_node(node.has_expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) defer_stmt(node ast.DeferStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('DeferStmt'))
	stmt_array := create_array()
	for s in node.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)
	to_object(obj, 'ifdef', t.string_node(node.ifdef))
	return obj
}

fn (t Tree) type_decl(node ast.TypeDecl) &C.cJSON {
	match node {
		ast.AliasTypeDecl { return t.alias_type_decl(node) }
		ast.SumTypeDecl { return t.sum_type_decl(node) }
		ast.FnTypeDecl { return t.fn_type_decl(node) }
	}
}

fn (t Tree) alias_type_decl(node ast.AliasTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AliasTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'parent_type', t.type_node(node.parent_type))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) sum_type_decl(node ast.SumTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SumTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	t_arr := create_array()
	for s in node.sub_types {
		to_array(t_arr, t.type_node(s))
	}
	to_object(obj, 'sub_types', t_arr)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) fn_type_decl(node ast.FnTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('FnTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_field(node ast.StructField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'comment', t.comment(node.comment))
	to_object(obj, 'default_expr', t.expr(node.default_expr))
	to_object(obj, 'has_default_expr', t.bool_node(node.has_default_expr))
	arr := create_array()
	for a in node.attrs {
		to_array(arr, t.string_node(a))
	}
	to_object(obj, 'attrs', arr)
	to_object(obj, 'is_public', t.bool_node(node.is_public))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) field(node ast.Field) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Field'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) arg(node table.Arg) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Arg'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	return obj
}

fn (t Tree) goto_label(node ast.GotoLabel) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GotoLabel'))
	to_object(obj, 'name', t.string_node(node.name))
	return obj
}

fn (t Tree) goto_stmt(node ast.GotoStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GotoStmt'))
	to_object(obj, 'name', t.string_node(node.name))
	return obj
}

fn (t Tree) lambda(node ast.Lambda) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Lambda'))
	to_object(obj, 'name', t.string_node(node.name))
	return obj
}

fn (t Tree) assign_stmt(node ast.AssignStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AssignStmt'))
	i_arr := create_array()
	for i in node.left {
		to_array(i_arr, t.expr(i))
	}
	to_object(obj, 'left', i_arr)
	e_arr := create_array()
	for e in node.right {
		to_array(e_arr, t.expr(e))
	}
	to_object(obj, 'right', e_arr)
	to_object(obj, 'op', t.number_node(int(node.op)))
	to_object(obj, 'pos', t.position(node.pos))
	lt_arr := create_array()
	for s in node.left_types {
		to_array(lt_arr, t.type_node(s))
	}
	to_object(obj, 'left_types', lt_arr)
	rt_arr := create_array()
	for s in node.left_types {
		to_array(rt_arr, t.type_node(s))
	}
	to_object(obj, 'right_types', rt_arr)
	to_object(obj, 'is_simple', t.bool_node(node.is_simple))
	to_object(obj, 'has_cross_var', t.bool_node(node.has_cross_var))
	return obj
}

fn (t Tree) var_decl(node ast.Var) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Var'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'is_arg', t.bool_node(node.is_arg))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'is_used', t.bool_node(node.is_used))
	return obj
}

fn (t Tree) return_(node ast.Return) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Return'))
	e_arr := create_array()
	for e in node.exprs {
		to_array(e_arr, t.expr(e))
	}
	to_object(obj, 'exprs', e_arr)
	to_object(obj, 'pos', t.position(node.pos))
	t_arr := create_array()
	for s in node.types {
		to_array(t_arr, t.type_node(s))
	}
	to_object(obj, 'types', t_arr)
	return obj
}

fn (t Tree) for_c_stmt(node ast.ForCStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ForCStmt'))
	to_object(obj, 'init', t.stmt(node.init))
	to_object(obj, 'has_init', t.bool_node(node.has_init))
	to_object(obj, 'cond', t.expr(node.cond))
	to_object(obj, 'has_cond', t.bool_node(node.has_cond))
	to_object(obj, 'inc', t.stmt(node.inc))
	to_object(obj, 'has_inc', t.bool_node(node.has_inc))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) for_stmt(node ast.ForStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ForStmt'))
	to_object(obj, 'cond', t.expr(node.cond))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'is_inf', t.bool_node(node.is_inf))
	return obj
}

fn (t Tree) for_in_stmt(node ast.ForInStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ForInStmt'))
	to_object(obj, 'key_var', t.string_node(node.key_var))
	to_object(obj, 'val_var', t.string_node(node.val_var))
	to_object(obj, 'cond', t.expr(node.cond))
	to_object(obj, 'is_range', t.bool_node(node.is_range))
	to_object(obj, 'high', t.expr(node.high))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'key_type', t.type_node(node.key_type))
	to_object(obj, 'val_type', t.type_node(node.val_type))
	to_object(obj, 'cond_type', t.type_node(node.cond_type))
	to_object(obj, 'kind', t.number_node(int(node.kind)))
	return obj
}

fn (t Tree) branch_stmt(node ast.BranchStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('BranchStmt'))
	to_object(obj, 'tok', t.number_node(int(&node.tok)))
	return obj
}

fn (t Tree) assert_stmt(node ast.AssertStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AssertStmt'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) unsafe_stmt(node ast.UnsafeStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('UnsafeStmt'))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	return obj
}

fn (t Tree) go_stmt(node ast.GoStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GoStmt'))
	to_object(obj, 'call_expr', t.expr(node.call_expr))
	return obj
}

fn (t Tree) block(node ast.Block) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Block'))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	return obj
}

fn (t Tree) comptime_call(node ast.ComptimeCall) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ComptimeCall'))
	to_object(obj, 'method_name', t.string_node(node.method_name))
	to_object(obj, 'left', t.expr(node.left))
	return obj
}

fn (t Tree) expr_stmt(node ast.ExprStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ExprStmt'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

// expr
fn (t Tree) expr(expr ast.Expr) &C.cJSON {
	match expr {
		ast.IntegerLiteral {
			return t.integer_literal(expr)
		}
		ast.FloatLiteral {
			return t.float_literal(expr)
		}
		ast.StringLiteral {
			return t.string_literal(expr)
		}
		ast.CharLiteral {
			return t.char_literal(expr)
		}
		ast.BoolLiteral {
			return t.bool_literal(expr)
		}
		ast.StringInterLiteral {
			return t.string_inter_literal(expr)
		}
		ast.EnumVal {
			return t.enum_val(expr)
		}
		ast.Assoc {
			return t.assoc(expr)
		}
		ast.CastExpr {
			return t.cast_expr(expr)
		}
		ast.AsCast {
			return t.as_cast(expr)
		}
		ast.Type {
			return t.type_expr(expr)
		}
		ast.SizeOf {
			return t.size_of(expr)
		}
		ast.PrefixExpr {
			return t.prefix_expr(expr)
		}
		ast.InfixExpr {
			return t.infix_expr(expr)
		}
		ast.IndexExpr {
			return t.index_expr(expr)
		}
		ast.PostfixExpr {
			return t.postfix_expr(expr)
		}
		ast.SelectorExpr {
			return t.selector_expr(expr)
		}
		ast.RangeExpr {
			return t.range_expr(expr)
		}
		ast.IfExpr {
			return t.if_expr(expr)
		}
		ast.Ident {
			return t.ident(expr)
		}
		ast.CallExpr {
			return t.call_expr(expr)
		}
		ast.OrExpr {
			return t.or_expr(expr)
		}
		ast.StructInit {
			return t.struct_init(expr)
		}
		ast.ArrayInit {
			return t.array_init(expr)
		}
		ast.MapInit {
			return t.map_init(expr)
		}
		ast.None {
			return t.none_expr(expr)
		}
		ast.ParExpr {
			return t.par_expr(expr)
		}
		ast.IfGuardExpr {
			return t.if_guard_expr(expr)
		}
		ast.MatchExpr {
			return t.match_expr(expr)
		}
		ast.ConcatExpr {
			return t.concat_expr(expr)
		}
		ast.TypeOf {
			return t.type_of(expr)
		}
		ast.Likely {
			return t.likely(expr)
		}
		ast.SqlExpr {
			return t.sql_expr(expr)
		}
		ast.SqlInsertExpr {
			return t.sql_insert_expr(expr)
		}
		else {
			// println('unknown expr')
			return t.null_node()
		}
	}
}

fn (t Tree) integer_literal(node ast.IntegerLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IntegerLiteral'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) float_literal(node ast.FloatLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('FloatLiteral'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_literal(node ast.StringLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StringLiteral'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'is_raw', t.bool_node(node.is_raw))
	to_object(obj, 'language', t.number_node(int(node.language)))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) char_literal(node ast.CharLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CharLiteral'))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) bool_literal(node ast.BoolLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('BoolLiteral'))
	to_object(obj, 'val', t.bool_node(node.val))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_inter_literal(node ast.StringInterLiteral) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StringInterLiteral'))
	v_arr := create_array()
	for v in node.vals {
		to_array(v_arr, t.string_node(v))
	}
	to_object(obj, 'vals', v_arr)
	e_arr := create_array()
	for e in node.exprs {
		to_array(e_arr, t.expr(e))
	}
	to_object(obj, 'exprs', e_arr)
	et_arr := create_array()
	for e in node.expr_types {
		to_array(et_arr, t.type_node(e))
	}
	to_object(obj, 'expr_types', et_arr)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) enum_val(node ast.EnumVal) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumVal'))
	to_object(obj, 'enum_name', t.string_node(node.enum_name))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) assoc(node ast.Assoc) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Assoc'))
	to_object(obj, 'var_name', t.string_node(node.var_name))
	s_arr := create_array()
	for f in node.fields {
		to_array(s_arr, t.string_node(f))
	}
	to_object(obj, 'fields', s_arr)
	e_arr := create_array()
	for e in node.exprs {
		to_array(e_arr, t.expr(e))
	}
	to_object(obj, 'exprs', e_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) cast_expr(node ast.CastExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CastExpr'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'arg', t.expr(node.arg))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typname', t.string_node(node.typname))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'has_arg', t.bool_node(node.has_arg))
	return obj
}

fn (t Tree) as_cast(node ast.AsCast) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AsCast'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_expr(node ast.Type) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Type'))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) size_of(node ast.SizeOf) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SizeOf'))
	to_object(obj, 'type_name', t.string_node(node.type_name))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) prefix_expr(node ast.PrefixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('PrefixExpr'))
	to_object(obj, 'op', t.number_node(int(node.op)))
	to_object(obj, 'right', t.expr(node.right))
	return obj
}

fn (t Tree) infix_expr(node ast.InfixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('InfixExpr'))
	to_object(obj, 'op', t.number_node(int(node.op)))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'right', t.expr(node.right))
	to_object(obj, 'right_type', t.type_node(node.right_type))
	return obj
}

fn (t Tree) index_expr(node ast.IndexExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IndexExpr'))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'index', t.expr(node.index))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'is_setter', t.bool_node(node.is_setter))
	return obj
}

fn (t Tree) postfix_expr(node ast.PostfixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('PostfixExpr'))
	to_object(obj, 'op', t.number_node(int(node.op)))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) selector_expr(node ast.SelectorExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SelectorExpr'))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'field_name', t.string_node(node.field_name))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	return obj
}

fn (t Tree) range_expr(node ast.RangeExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('RangeExpr'))
	to_object(obj, 'low', t.expr(node.low))
	to_object(obj, 'high', t.expr(node.high))
	return obj
}

fn (t Tree) if_expr(node ast.IfExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfExpr'))
	to_object(obj, 'tok_kind', t.number_node(int(node.tok_kind)))
	branch_arr := create_array()
	for b in node.branches {
		to_array(branch_arr, t.if_branch(b))
	}
	to_object(obj, 'branches', branch_arr)
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'has_else', t.bool_node(node.has_else))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	return obj
}

fn (t Tree) if_branch(node ast.IfBranch) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfBranch'))
	to_object(obj, 'cond', t.expr(node.cond))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'comment', t.comment(node.comment))
	return obj
}

fn (t Tree) ident(node ast.Ident) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Ident'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'value', t.string_node(node.value))
	to_object(obj, 'language', t.number_node(int(node.language)))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'tok_kind', t.number_node(int(node.tok_kind)))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'kind', t.number_node(int(node.kind)))
	to_object(obj, 'info', t.ident_info(node.info))
	return obj
}

fn (t Tree) ident_info(info ast.IdentInfo) &C.cJSON {
	match info {
		ast.IdentVar { return t.ident_var(info) }
		ast.IdentFn { return t.ident_fn(info) }
	}
}

fn (t Tree) ident_var(node ast.IdentVar) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IdentVar'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'is_static', t.bool_node(node.is_static))
	to_object(obj, 'is_optional', t.bool_node(node.is_optional))
	return obj
}

fn (t Tree) ident_fn(node ast.IdentFn) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IdentFn'))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) call_expr(node ast.CallExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CallExpr'))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'is_method', t.bool_node(node.is_method))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'language', t.number_node(int(node.language)))
	arg_arr := create_array()
	for e in node.args {
		to_array(arg_arr, t.call_arg(e))
	}
	to_object(obj, 'args', arg_arr)
	t_arr := create_array()
	for e in node.expected_arg_types {
		to_array(t_arr, t.type_node(e))
	}
	to_object(obj, 'expected_arg_types', t_arr)
	to_object(obj, 'or_block', t.or_expr(node.or_block))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'receiver_type', t.type_node(node.receiver_type))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'generic_type', t.type_node(node.generic_type))
	return obj
}

fn (t Tree) call_arg(node ast.CallArg) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CallArg'))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) or_expr(node ast.OrExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('OrExpr'))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'kind', t.number_node(int(node.kind)))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_init(node ast.StructInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructInit'))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_short', t.bool_node(node.is_short))
	s_arr := create_array()
	for f in node.fields {
		to_array(s_arr, t.struct_init_field(f))
	}
	to_object(obj, 'fields', s_arr)
	return obj
}

fn (t Tree) struct_init_field(node ast.StructInitField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructInitField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'expected_type', t.type_node(node.expected_type))
	return obj
}

fn (t Tree) array_init(node ast.ArrayInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ArrayInit'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'elem_type', t.type_node(node.elem_type))
	expr_arr := create_array()
	for e in node.exprs {
		to_array(expr_arr, t.expr(e))
	}
	to_object(obj, 'exprs', expr_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'elem_type_pos', t.position(node.elem_type_pos))
	to_object(obj, 'is_fixed', t.bool_node(node.is_fixed))
	to_object(obj, 'has_val', t.bool_node(node.has_val))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'len_expr', t.expr(node.len_expr))
	to_object(obj, 'cap_expr', t.expr(node.cap_expr))
	to_object(obj, 'default_expr', t.expr(node.default_expr))
	to_object(obj, 'has_len', t.bool_node(node.has_len))
	to_object(obj, 'has_cap', t.bool_node(node.has_cap))
	to_object(obj, 'has_default', t.bool_node(node.has_default))
	to_object(obj, 'is_interface', t.bool_node(node.is_interface))
	to_object(obj, 'interface_type', t.type_node(node.interface_type))
	i_arr := create_array()
	for i in node.interface_types {
		to_array(i_arr, t.type_node(i))
	}
	to_object(obj, 'interface_types', i_arr)
	return obj
}

fn (t Tree) map_init(node ast.MapInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('MapInit'))
	to_object(obj, 'typ', t.type_node(node.typ))
	k_arr := create_array()
	for k in node.keys {
		to_array(k_arr, t.expr(k))
	}
	to_object(obj, 'keys', k_arr)
	v_arr := create_array()
	for v in node.vals {
		to_array(v_arr, t.expr(v))
	}
	to_object(obj, 'vals', v_arr)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) none_expr(node ast.None) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('None'))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'foo', t.number_node(node.foo))
	return obj
}

fn (t Tree) par_expr(node ast.ParExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ParExpr'))
	to_object(obj, 'expr', t.expr(node.expr))
	return obj
}

fn (t Tree) if_guard_expr(node ast.IfGuardExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfGuardExpr'))
	to_object(obj, 'var_name', t.string_node(node.var_name))
	to_object(obj, 'expr', t.expr(node.expr))
	return obj
}

fn (t Tree) match_expr(node ast.MatchExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('MatchExpr'))
	to_object(obj, 'tok_kind', t.number_node(int(node.tok_kind)))
	to_object(obj, 'cond', t.expr(node.cond))
	m_arr := create_array()
	for b in node.branches {
		to_array(m_arr, t.match_branch(b))
	}
	to_object(obj, 'branches', m_arr)
	to_object(obj, 'cond_type', t.type_node(node.cond_type))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'is_sum_type', t.bool_node(node.is_sum_type))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'var_name', t.string_node(node.var_name))
	return obj
}

fn (t Tree) match_branch(node ast.MatchBranch) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('MatchBranch'))
	expr_arr := create_array()
	for e in node.exprs {
		to_array(expr_arr, t.expr(e))
	}
	to_object(obj, 'exprs', expr_arr)
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'comment', t.comment(node.comment))
	to_object(obj, 'is_else', t.bool_node(node.is_else))
	return obj
}

fn (t Tree) concat_expr(node ast.ConcatExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ConcatExpr'))
	expr_arr := create_array()
	for e in node.vals {
		to_array(expr_arr, t.expr(e))
	}
	to_object(obj, 'vals', expr_arr)
	to_object(obj, 'return_type', t.type_node(node.return_type))
	return obj
}

fn (t Tree) type_of(node ast.TypeOf) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('TypeOf'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	return obj
}

fn (t Tree) likely(node ast.Likely) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Likely'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'is_likely', t.bool_node(node.is_likely))
	return obj
}

fn (t Tree) sql_expr(node ast.SqlExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'type', t.type_node(node.typ))
	to_object(obj, 'is_count', t.bool_node(node.is_count))
	to_object(obj, 'db_var_name', t.string_node(node.db_var_name))
	to_object(obj, 'table_name', t.string_node(node.table_name))
	to_object(obj, 'where_expr', t.expr(node.where_expr))
	to_object(obj, 'has_where', t.bool_node(node.has_where))
	to_object(obj, 'is_array', t.bool_node(node.is_array))
	field_arr := create_array()
	for f in node.fields {
		to_array(field_arr, t.table_field(f))
	}
	to_object(obj, 'fields', field_arr)
	return obj
}

fn (t Tree) table_field(node table.Field) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('TableField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'default_expr', t.expr(ast.fe2ex(node.default_expr)))
	to_object(obj, 'has_default_expr', t.bool_node(node.has_default_expr))
	arr := create_array()
	for a in node.attrs {
		to_array(arr, t.string_node(a))
	}
	to_object(obj, 'attrs', arr)
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'is_global', t.bool_node(node.is_global))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) sql_insert_expr(node ast.SqlInsertExpr) &C.cJSON {
	obj := create_object()
	return obj
}

[inline]
fn to_object(node &C.cJSON, key string, child &C.cJSON) {
	add_item_to_object(node, key, child)
}

[inline]
fn to_array(node, child &C.cJSON) {
	add_item_to_array(node, child)
}

// get absolute path for file
fn abs_path(path string) string {
	if os.is_abs_path(path) {
		return path
	} else if path.starts_with('./') {
		return os.join_path(os.getwd(), path[2..])
	} else {
		return os.join_path(os.getwd(), path)
	}
}
