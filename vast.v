module main

import v.token
import v.parser
import v.table
import v.ast
import v.pref
import v.errors
import os
import time

const version = '0.2.2' // the version of vast will follow vlang

const usage   = '
usage:
  1.vast demo.v 	 generate demo.json file.
  2.vast -w demo.v 	 generate demo.json file, and watch.
  3.vast -c domo.v 	 generate deom.json and demo.c file, and watch.
  4.vast -p demo.v 	 print the json string to termial.'

fn main() {
	args := os.args.clone()
	match args.len {
		2 {
			file := get_abs_path(args[1])
			check_file(file)
			println('AST written to: ' + json_file(file))
		}
		3 {
			file := get_abs_path(args[2])
			check_file(file)
			option := args[1]
			match option {
				'-p' { println(json(file)) }
				'-w' { gen(file, false) }
				'-c' { gen(file, true) }
				else { println(usage) }
			}
		}
		else {
			println(usage)
		}
	}
}

// generate ast json file and c source code file
fn gen(file string, is_genc bool) {
	println('start watching...')
	mut timestamp := 0
	for {
		new_timestamp := os.file_last_mod_unix(file)
		if timestamp != new_timestamp {
			res := json_file(file)
			println('$time.now() : AST written to: ' + res)
			if is_genc {
				file_name := file[0..(file.len - os.file_ext(file).len)]
				os.system('v -o ${file_name}.c $file')
			}
		}
		timestamp = new_timestamp
		time.sleep(500*time.millisecond)
	}
}

// get absolute path for file
fn get_abs_path(path string) string {
	if os.is_abs_path(path) {
		return path
	} else if path.starts_with('./') {
		return os.join_path(os.getwd(), path[2..])
	} else {
		return os.join_path(os.getwd(), path)
	}
}

// check file is v file and exists
fn check_file(file string) {
	if os.file_ext(file) !in ['.v', '.vsh'] {
		panic('the file `$file` must be a v file or vsh file')
	}
	if !os.exists(file) {
		panic('the v file `$file` does not exist')
	}
}

struct Tree {
	table        &table.Table
	pref         &pref.Preferences
	global_scope &ast.Scope
mut:
	root &C.cJSON // the root of tree
}

// generate json file with the same file name
fn json_file(file string) string {
	ast_json := json(file)
	// support .v and .vsh file
	file_name := file[0..(file.len - os.file_ext(file).len)]
	json_file := file_name + '.json'
	os.write_file(json_file, ast_json) or { panic(err) }
	return json_file
}

// for enable_globals
fn new_preferences() &pref.Preferences {
	mut p := &pref.Preferences{}
	p.fill_with_defaults()
	p.enable_globals = true
	return p
}

// generate json string
fn json(file string) string {
	mut t := Tree{
		root: create_object()
		table: table.new_table()
		pref: new_preferences()
		global_scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
	}
	// parse file with comment
	ast_file := parser.parse_file(file, t.table, .parse_comments, t.pref, t.global_scope)
	// parse file without comment
	// ast_file := parser.parse_file(file, t.table, .skip_comments, t.pref, t.global_scope)
	t.root = t.ast_file(ast_file)
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

//todo:try to simplify the array node
//array type node
fn (t Tree) array_node(nodes []ast.Node) &C.cJSON {
	mut arr:=create_array()
	for n in nodes {
		match n  {
			ast.Stmt {
				to_array(arr,t.stmt(n))
			}
			ast.Expr {
				to_array(arr,t.expr(n))
			}
			ast.StructField {
				to_array(arr,t.struct_field(n))
			}
			ast.StructInitField {
				to_array(arr,t.struct_init_field(n))
			}
			ast.EnumField {
				to_array(arr,t.enum_field(n))
			}
			ast.Field {
				to_array(arr,t.field(n))
			}
			ast.IfBranch {
				to_array(arr,t.if_branch(n))
			}
			ast.MatchBranch {
				to_array(arr,t.match_branch(n))
			}
			ast.SelectBranch {
				to_array(arr,t.select_branch(n))
			}
			ast.ScopeObject {
				to_array(arr,t.scope_object(n))
			}
			table.Param {
				to_array(arr,t.arg(n))
			}
			else {
				panic('node must be type of ast.Node')
			}
		}
	}
	return arr
}

// type type node
fn (t Tree) type_node(typ table.Type) &C.cJSON {
	if typ == 0 {
		return create_null()
	} else {
		type_name := t.table.get_type_name(typ)
		return create_string(type_name)
	}
}

// token type node
fn (t Tree) token_node(tok_kind token.Kind) &C.cJSON {
	return t.string_node('token:${int(tok_kind)}($tok_kind.str())')
}

// enum type node
fn (t Tree) enum_node<T>(value T) &C.cJSON {
	return t.string_node('enum:${int(value)}($value)')
}

// ast file root node
fn (t Tree) ast_file(ast_file ast.File) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ast.File'))
	to_object(obj, 'path', t.string_node(ast_file.path))
	to_object(obj, 'path_base', t.string_node(ast_file.path_base))
	to_object(obj, 'mod', t.mod(ast_file.mod))
	to_object(obj, 'imports', t.imports(ast_file.imports))
	to_object(obj, 'global_scope', t.scope(ast_file.global_scope))
	to_object(obj, 'scope', t.scope(ast_file.scope))
	to_object(obj, 'errors', t.errors(ast_file.errors))
	to_object(obj, 'warnings', t.warnings(ast_file.warnings))
	auto_import_array := create_array()
	for i in ast_file.auto_imports {
		to_array(auto_import_array, t.string_node(i))
	}
	to_object(obj, 'auto_imports', auto_import_array)
	//
	symbol_obj := create_object()
	for key, val in ast_file.imported_symbols {
		to_object(symbol_obj, key, t.string_node(val))
	}
	to_object(obj, 'imported_symbols', symbol_obj)
	//
	fn_array := create_array()
	for f in ast_file.generic_fns {
		to_array(fn_array, t.fn_decl(f))
	}
	to_object(obj, 'generic_fns', fn_array)
	//
	embed_file_array := create_array()
	for e in ast_file.embedded_files {
		to_array(embed_file_array, t.embed_file(e))
	}
	to_object(obj, 'embedded_files', embed_file_array)
	//
	to_object(obj, 'stmts', t.stmts(ast_file.stmts))
	return obj
}

// embed files
fn (t Tree) embed_file(node ast.EmbeddedFile) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EmbeddedFile'))
	to_object(obj, 'rpath', t.string_node(node.rpath))
	to_object(obj, 'apath', t.string_node(node.apath))
	return obj
}

// ast module node
fn (t Tree) mod(node ast.Module) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Module'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'short_name', t.string_node(node.short_name))
	to_object(obj, 'is_skipped', t.bool_node(node.is_skipped))
	a_arr := create_array()
	for a in node.attrs {
		to_array(a_arr, t.attr(a))
	}
	to_object(obj, 'attrs', a_arr)
	to_object(obj, 'pos', t.position(node.pos))
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
	struct_field_array := create_array()
	for s in scope.struct_fields {
		to_array(struct_field_array, t.scope_struct_field(s))
	}
	to_object(obj, 'struct_fields', struct_field_array)
	return obj
}

fn (t Tree) scope_struct_field(node ast.ScopeStructField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ScopeStructField'))
	to_object(obj, 'struct_type', t.type_node(node.struct_type))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'orig_type', t.type_node(node.orig_type))
	to_object(obj, 'pos', t.position(node.pos))
	type_array := create_array()
	for typ in node.sum_type_casts {
		to_array(type_array, t.type_node(typ))
	}
	to_object(obj, 'sum_type_casts', type_array)
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
		ast.GlobalField { t.global_field(node) }
		ast.Var { t.var(node) }
		ast.AsmRegister { t.asm_register(node) }
	}
	return obj
}

fn (t Tree) imports(imports []ast.Import) &C.cJSON {
	import_array := create_array()
	for imp in imports {
		to_array(import_array, t.import_module(imp))
	}
	return import_array
}

fn (t Tree) errors(errors []errors.Error) &C.cJSON {
	errs := create_array()
	for e in errors {
		obj := create_object()
		to_object(obj, 'message', t.string_node(e.message))
		to_object(obj, 'file_path', t.string_node(e.file_path))
		to_object(obj, 'pos', t.position(e.pos))
		to_object(obj, 'backtrace', t.string_node(e.backtrace))
		to_object(obj, 'reporter', t.enum_node(e.reporter))
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
		to_object(obj, 'reporter', t.enum_node(w.reporter))
		to_array(warns, obj)
	}
	return warns
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
		ast.Import { return t.import_module(node) }
		ast.ConstDecl { return t.const_decl(node) }
		ast.FnDecl { return t.fn_decl(node) }
		ast.StructDecl { return t.struct_decl(node) }
		ast.EnumDecl { return t.enum_decl(node) }
		ast.InterfaceDecl { return t.interface_decl(node) }
		ast.HashStmt { return t.hash_stmt(node) }
		ast.CompFor { return t.comp_for(node) }
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
		ast.ExprStmt { return t.expr_stmt(node) }
		ast.GoStmt { return t.go_stmt(node) }
		ast.Block { return t.block(node) }
		ast.SqlStmt { return t.sql_stmt(node) }
		ast.AsmStmt {return t.asm_stmt(node)}
	}
	// fixed ForCStmt without init stmt
	return t.null_node()
}

fn (t Tree) import_module(node ast.Import) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Import'))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'alias', t.string_node(node.alias))
	syms := create_array()
	for s in node.syms {
		to_array(syms, t.import_symbol(s))
	}
	to_object(obj, 'syms', syms)

	
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)

	//todo:try to simplify the array node
	// to_object(obj, 'comments', t.array_node(node.comments))

	next_comment_array := create_array()
	for c in node.next_comments {
		to_array(next_comment_array, t.comment(c))
	}
	to_object(obj, 'next_comments', next_comment_array)

	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) import_symbol(node ast.ImportSymbol) &C.cJSON {
	obj := create_object()
	to_object(obj, 'name', t.string_node(node.name))
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
	to_object(obj, 'is_multi', t.bool_node(node.is_multi))
	to_object(obj, 'line_nr', t.number_node(node.line_nr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_decl(node ast.ConstDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ConstDecl'))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_block', t.bool_node(node.is_block))
	field_array := create_array()
	for f in node.fields {
		to_array(field_array, t.const_field(f))
	}
	to_object(obj, 'fields', field_array)
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.end_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'end_comments', comment_array)
	return obj
}

fn (t Tree) const_field(node ast.ConstField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ConstField'))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	return obj
}

// function declaration
fn (t Tree) fn_decl(node ast.FnDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('FnDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'is_deprecated', t.bool_node(node.is_deprecated))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_variadic', t.bool_node(node.is_variadic))
	to_object(obj, 'is_anon', t.bool_node(node.is_anon))
	to_object(obj, 'is_manualfree', t.bool_node(node.is_manualfree))
	to_object(obj, 'is_main', t.bool_node(node.is_main))
	to_object(obj, 'is_test', t.bool_node(node.is_test))
	to_object(obj, 'is_conditional', t.bool_node(node.is_conditional))
	to_object(obj, 'receiver', t.field(node.receiver))
	to_object(obj, 'receiver_pos', t.position(node.receiver_pos))
	to_object(obj, 'is_method', t.bool_node(node.is_method))
	to_object(obj, 'method_idx', t.number_node(node.method_idx))
	to_object(obj, 'rec_mut', t.bool_node(node.rec_mut))
	to_object(obj, 'rec_share', t.enum_node(node.rec_share))
	to_object(obj, 'language', t.enum_node(node.language))
	to_object(obj, 'no_body', t.bool_node(node.no_body))
	to_object(obj, 'is_builtin', t.bool_node(node.is_builtin))
	to_object(obj, 'is_direct_arr', t.bool_node(node.is_direct_arr))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'body_pos', t.position(node.body_pos))
	to_object(obj, 'file', t.string_node(node.file))
	to_object(obj, 'has_return', t.bool_node(node.has_return))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'source_file', t.number_node(int(node.source_file)))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	to_object(obj, 'skip_gen', t.bool_node(node.skip_gen))
	a_arr := create_array()
	for a in node.attrs {
		to_array(a_arr, t.attr(a))
	}
	to_object(obj, 'attrs', a_arr)

	params_array := create_array()
	for a in node.params {
		to_array(params_array, t.arg(a))
	}
	to_object(obj, 'params', params_array)

	generic_array := create_array()
	for g in node.generic_params {
		to_array(generic_array, t.generic_param(g))
	}
	to_object(obj, 'generic_params', generic_array)

	stmt_array := create_array()
	for s in node.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)

	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)

	next_comment_array := create_array()
	for c in node.next_comments {
		to_array(next_comment_array, t.comment(c))
	}
	to_object(obj, 'next_comments', next_comment_array)

	label_name_array := create_array()
	for l in node.label_names {
		to_array(label_name_array, t.string_node(l))
	}
	to_object(obj, 'label_names', label_name_array)

	defer_stmts_array := create_array()
	for stmt in node.defer_stmts {
		to_array(defer_stmts_array, t.defer_stmt(stmt))
	}
	to_object(obj, 'defer_stmts', defer_stmts_array)

	return obj
}

fn (t Tree) generic_param(node ast.GenericParam) &C.cJSON {
	obj := create_object()
	to_object(obj, 'name', t.string_node(node.name))
	return obj
}

fn (t Tree) anon_fn(node ast.AnonFn) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AnonFn'))
	to_object(obj, 'decl', t.fn_decl(node.decl))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'has_gen', t.bool_node(node.has_gen))
	return obj
}

fn (t Tree) struct_decl(node ast.StructDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'pub_pos', t.number_node(node.pub_pos))
	to_object(obj, 'mut_pos', t.number_node(node.mut_pos))
	to_object(obj, 'pub_mut_pos', t.number_node(node.pub_mut_pos))
	to_object(obj, 'global_pos', t.number_node(node.global_pos))
	to_object(obj, 'module_pos', t.number_node(node.module_pos))
	to_object(obj, 'language', t.enum_node(node.language))
	to_object(obj, 'is_union', t.bool_node(node.is_union))
	to_object(obj, 'pos', t.position(node.pos))
	f_array := create_array()
	for f in node.fields {
		to_array(f_array, t.struct_field(f))
	}
	to_object(obj, 'fields', f_array)
	gen_type_array := create_array()
	for typ in node.gen_types {
		to_array(gen_type_array, t.type_node(typ))
	}
	to_object(obj, 'gen_types', gen_type_array)
	a_array := create_array()
	for a in node.attrs {
		to_array(a_array, t.attr(a))
	}
	to_object(obj, 'attrs', a_array)
	c_array := create_array()
	for c in node.end_comments {
		to_array(c_array, t.comment(c))
	}
	to_object(obj, 'end_comments', c_array)
	embed_array := create_array()
	for e in node.embeds {
		to_array(embed_array, t.embed(e))
	}
	to_object(obj, 'embeds', embed_array)
	return obj
}

fn (t Tree) embed(node ast.Embed) &C.cJSON {
	obj := create_object()
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) enum_decl(node ast.EnumDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_flag', t.bool_node(node.is_flag))
	to_object(obj, 'is_multi_allowed', t.bool_node(node.is_multi_allowed))
	to_object(obj, 'pos', t.position(node.pos))
	f_arr := create_array()
	for f in node.fields {
		to_array(f_arr, t.enum_field(f))
	}
	to_object(obj, 'fields', f_arr)
	c_array := create_array()
	for c in node.comments {
		to_array(c_array, t.comment(c))
	}
	to_object(obj, 'comments', c_array)
	attr_array := create_array()
	for a in node.attrs {
		to_array(attr_array, t.attr(a))
	}
	to_object(obj, 'attrs', attr_array)
	return obj
}

fn (t Tree) enum_field(node ast.EnumField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'has_expr', t.bool_node(node.has_expr))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	next_comment_array := create_array()
	for c in node.next_comments {
		to_array(next_comment_array, t.comment(c))
	}
	to_object(obj, 'next_comments', next_comment_array)
	return obj
}

fn (t Tree) interface_decl(node ast.InterfaceDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('InterfaceDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'mut_pos', t.number_node(node.mut_pos))
	str_array := create_array()
	for s in node.field_names {
		to_array(str_array, t.string_node(s))
	}
	to_object(obj, 'field_names', str_array)

	m_array := create_array()
	for m in node.methods {
		to_array(m_array, t.fn_decl(m))
	}
	to_object(obj, 'methods', m_array)

	f_array := create_array()
	for f in node.fields {
		to_array(f_array, t.struct_field(f))
	}
	to_object(obj, 'fields', f_array)

	comment_array := create_array()
	for c in node.pre_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'pre_comments', comment_array)

	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) attr(node table.Attr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Attr'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_string', t.bool_node(node.is_string))
	to_object(obj, 'is_comptime_define', t.bool_node(node.is_comptime_define))
	to_object(obj, 'arg', t.string_node(node.arg))
	to_object(obj, 'is_string_arg', t.bool_node(node.is_string_arg))
	return obj
}

fn (t Tree) hash_stmt(node ast.HashStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('HashStmt'))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'kind', t.string_node(node.kind))
	to_object(obj, 'main', t.string_node(node.main))
	to_object(obj, 'msg', t.string_node(node.msg))
	to_object(obj, 'source_file', t.string_node(node.source_file))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) comp_for(node ast.CompFor) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CompFor'))
	to_object(obj, 'val_var', t.string_node(node.val_var))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ_pos', t.position(node.pos))
	stmt_array := create_array()
	for s in node.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)
	return obj
}

fn (t Tree) global_decl(node ast.GlobalDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GlobalDecl'))
	to_object(obj, 'pos', t.position(node.pos))
	field_array := create_array()
	for f in node.fields {
		to_array(field_array, t.global_field(f))
	}
	to_object(obj, 'fields', field_array)
	comment_array := create_array()
	for c in node.end_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'end_comments', comment_array)
	return obj
}

fn (t Tree) global_field(node ast.GlobalField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GlobalField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'has_expr', t.bool_node(node.has_expr))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
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
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_decl(node ast.TypeDecl) &C.cJSON {
	match node {
		ast.AliasTypeDecl { return t.alias_type_decl(node) }
		ast.FnTypeDecl { return t.fn_type_decl(node) }
		ast.SumTypeDecl { return t.sum_type_decl(node) }
	}
}

fn (t Tree) alias_type_decl(node ast.AliasTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AliasTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'parent_type', t.type_node(node.parent_type))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) sum_type_decl(node ast.SumTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SumTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'typ', t.type_node(node.typ))
	// comments
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	// variants
	t_array := create_array()
	for s in node.variants {
		variants_obj := create_object()
		to_object(variants_obj, 'typ', t.type_node(s.typ))
		to_object(variants_obj, 'pos', t.position(s.pos))
		to_array(t_array, variants_obj)
	}
	to_object(obj, 'variants', t_array)
	return obj
}

fn (t Tree) fn_type_decl(node ast.FnTypeDecl) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('FnTypeDecl'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	// comments
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	return obj
}

fn (t Tree) struct_field(node ast.StructField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_public', t.bool_node(node.is_public))
	to_object(obj, 'has_default_expr', t.bool_node(node.has_default_expr))
	to_object(obj, 'default_expr', t.expr(node.default_expr))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'type_pos', t.position(node.type_pos))
	attr_array := create_array()
	for a in node.attrs {
		to_array(attr_array, t.attr(a))
	}
	to_object(obj, 'attrs', attr_array)
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
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

fn (t Tree) arg(node table.Param) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Param'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	return obj
}

fn (t Tree) goto_label(node ast.GotoLabel) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GotoLabel'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) goto_stmt(node ast.GotoStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GotoStmt'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) assign_stmt(node ast.AssignStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AssignStmt'))
	left_array := create_array()
	for i in node.left {
		to_array(left_array, t.expr(i))
	}
	to_object(obj, 'left', left_array)
	left_type_array := create_array()
	for s in node.left_types {
		to_array(left_type_array, t.type_node(s))
	}
	to_object(obj, 'left_types', left_type_array)
	right_array := create_array()
	for e in node.right {
		to_array(right_array, t.expr(e))
	}
	to_object(obj, 'right', right_array)
	right_type_array := create_array()
	for s in node.left_types {
		to_array(right_type_array, t.type_node(s))
	}
	to_object(obj, 'right_types', right_type_array)
	to_object(obj, 'op', t.token_node(node.op))
	to_object(obj, 'is_static', t.bool_node(node.is_static))
	to_object(obj, 'is_simple', t.bool_node(node.is_simple))
	to_object(obj, 'has_cross_var', t.bool_node(node.has_cross_var))
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	end_comment_array := create_array()
	for c in node.end_comments {
		to_array(end_comment_array, t.comment(c))
	}
	to_object(obj, 'end_comments', end_comment_array)
	return obj
}

fn (t Tree) var(node ast.Var) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Var'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'orig_type', t.type_node(node.orig_type))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_arg', t.bool_node(node.is_arg))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'is_used', t.bool_node(node.is_used))
	to_object(obj, 'is_changed', t.bool_node(node.is_changed))
	to_object(obj, 'is_or', t.bool_node(node.is_or))
	to_object(obj, 'is_tmp', t.bool_node(node.is_tmp))
	to_object(obj, 'is_autofree_tmp', t.bool_node(node.is_autofree_tmp))
	to_object(obj, 'is_auto_deref', t.bool_node(node.is_auto_deref))
	to_object(obj, 'share', t.enum_node(node.share))
	to_object(obj, 'pos', t.position(node.pos))
	type_array := create_array()
	for typ in node.sum_type_casts {
		to_array(type_array, t.type_node(typ))
	}
	to_object(obj, 'sum_type_casts', type_array)
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
	t_arr := create_array()
	for s in node.types {
		to_array(t_arr, t.type_node(s))
	}
	to_object(obj, 'types', t_arr)
	to_object(obj, 'pos', t.position(node.pos))
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
	to_object(obj, 'is_multi', t.bool_node(node.is_multi))
	to_object(obj, 'label', t.string_node(node.label))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	return obj
}

fn (t Tree) for_stmt(node ast.ForStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ForStmt'))
	to_object(obj, 'cond', t.expr(node.cond))
	to_object(obj, 'is_inf', t.bool_node(node.is_inf))
	to_object(obj, 'label', t.string_node(node.label))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
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
	to_object(obj, 'key_type', t.type_node(node.key_type))
	to_object(obj, 'val_type', t.type_node(node.val_type))
	to_object(obj, 'cond_type', t.type_node(node.cond_type))
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'val_is_mut', t.bool_node(node.val_is_mut))
	to_object(obj, 'label', t.string_node(node.label))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	return obj
}

fn (t Tree) branch_stmt(node ast.BranchStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('BranchStmt'))
	to_object(obj, 'kind', t.token_node(node.kind))
	to_object(obj, 'label', t.string_node(node.label))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) assert_stmt(node ast.AssertStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AssertStmt'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) go_stmt(node ast.GoStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GoStmt'))
	to_object(obj, 'call_expr', t.call_expr(node.call_expr))
	to_object(obj, 'pos', t.position(node.pos))
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
	to_object(obj, 'is_unsafe', t.bool_node(node.is_unsafe))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) comptime_call(node ast.ComptimeCall) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ComptimeCall'))
	to_object(obj, 'method_name', t.string_node(node.method_name))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'is_vweb', t.bool_node(node.is_vweb))
	to_object(obj, 'vweb_tmpl', t.string_node(node.vweb_tmpl.path))
	to_object(obj, 'args_var', t.string_node(node.args_var))
	to_object(obj, 'sym', t.string_node(node.sym.name))
	to_object(obj, 'has_parens', t.bool_node(node.has_parens))
	to_object(obj, 'is_embed', t.bool_node(node.is_embed))
	to_object(obj, 'embed_file', t.embed_file(node.embed_file))
	to_object(obj, 'method_pos', t.position(node.method_pos))
	to_object(obj, 'result_type', t.type_node(node.result_type))
	to_object(obj, 'scope', t.scope(node.scope))
	to_object(obj, 'env_value', t.string_node(node.env_value))
	to_object(obj, 'pos', t.position(node.pos))
	arg_array := create_array()
	for e in node.args {
		to_array(arg_array, t.call_arg(e))
	}
	to_object(obj, 'args', arg_array)
	
	return obj
}

fn (t Tree) comptime_selector(node ast.ComptimeSelector) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ComptimeSelector'))
	to_object(obj, 'has_parens', t.bool_node(node.has_parens))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'field_expr', t.expr(node.field_expr))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) expr_stmt(node ast.ExprStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ExprStmt'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
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
		ast.AtExpr {
			return t.at_expr(expr)
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
		ast.ComptimeCall {
			return t.comptime_call(expr)
		}
		ast.ComptimeSelector {
			return t.comptime_selector(expr)
		}
		ast.LockExpr {
			return t.lock_expr(expr)
		}
		ast.UnsafeExpr {
			return t.unsafe_expr(expr)
		}
		ast.ChanInit {
			return t.chan_init(expr)
		}
		ast.SelectExpr {
			return t.select_expr(expr)
		}
		ast.Comment {
			return t.comment(expr)
		}
		ast.AnonFn {
			return t.anon_fn(expr)
		}
		ast.ArrayDecompose {
			return t.array_decompose(expr)
		}
		ast.GoExpr {
			return t.go_expr(expr)
		}
		ast.OffsetOf {
			return t.offset_of(expr)
		}
		ast.DumpExpr {
			return t.dump_expr(expr)
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
	to_object(obj, 'language', t.enum_node(node.language))
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
	//
	e_arr := create_array()
	for e in node.exprs {
		to_array(e_arr, t.expr(e))
	}
	to_object(obj, 'exprs', e_arr)
	//
	et_arr := create_array()
	for e in node.expr_types {
		to_array(et_arr, t.type_node(e))
	}
	to_object(obj, 'expr_types', et_arr)
	//
	fw_arr := create_array()
	for fw in node.fwidths {
		to_array(fw_arr, t.number_node(fw))
	}
	to_object(obj, 'fwidths', fw_arr)
	//
	p_arr := create_array()
	for p in node.precisions {
		to_array(p_arr, t.number_node(p))
	}
	to_object(obj, 'precisions', p_arr)
	//
	pl_arr := create_array()
	for p in node.pluss {
		to_array(pl_arr, t.bool_node(p))
	}
	to_object(obj, 'pluss', pl_arr)
	//
	f_arr := create_array()
	for f in node.fills {
		to_array(f_arr, t.bool_node(f))
	}
	to_object(obj, 'fills', f_arr)
	//
	poss_arr := create_array()
	for p in node.fmt_poss {
		to_array(poss_arr, t.position(p))
	}
	to_object(obj, 'fmt_poss', poss_arr)
	//
	fmts_arr := create_array()
	for f in node.fmts {
		to_array(fmts_arr, t.number_node(int(f)))
	}
	to_object(obj, 'fmts', fmts_arr)
	//
	n_arr := create_array()
	for n in node.need_fmts {
		to_array(n_arr, t.bool_node(n))
	}
	to_object(obj, 'need_fmts', n_arr)
	//
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) enum_val(node ast.EnumVal) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('EnumVal'))
	to_object(obj, 'enum_name', t.string_node(node.enum_name))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'val', t.string_node(node.val))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
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
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) at_expr(node ast.AtExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('AtExpr'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'val', t.string_node(node.val))
	return obj
}

fn (t Tree) cast_expr(node ast.CastExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CastExpr'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'arg', t.expr(node.arg))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'typname', t.string_node(node.typname))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'has_arg', t.bool_node(node.has_arg))
	to_object(obj, 'in_prexpr', t.bool_node(node.in_prexpr))
	to_object(obj, 'pos', t.position(node.pos))
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
	to_object(obj, 'is_type', t.bool_node(node.is_type))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) prefix_expr(node ast.PrefixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('PrefixExpr'))
	to_object(obj, 'op', t.token_node(node.op))
	to_object(obj, 'right', t.expr(node.right))
	to_object(obj, 'right_type', t.type_node(node.right_type))
	to_object(obj, 'or_block', t.or_expr(node.or_block))
	to_object(obj, 'is_option', t.bool_node(node.is_option))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) infix_expr(node ast.InfixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('InfixExpr'))
	to_object(obj, 'op', t.token_node(node.op))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'right', t.expr(node.right))
	to_object(obj, 'right_type', t.type_node(node.right_type))
	to_object(obj, 'auto_locked', t.string_node(node.auto_locked))
	to_object(obj, 'or_block', t.or_expr(node.or_block))
	to_object(obj, 'is_stmt', t.bool_node(node.is_stmt))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) index_expr(node ast.IndexExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IndexExpr'))
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'index', t.expr(node.index))
	to_object(obj, 'is_setter', t.bool_node(node.is_setter))
	to_object(obj, 'or_expr', t.or_expr(node.or_expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) postfix_expr(node ast.PostfixExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('PostfixExpr'))
	to_object(obj, 'op', t.token_node(node.op))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'auto_locked', t.string_node(node.auto_locked))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) selector_expr(node ast.SelectorExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SelectorExpr'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'field_name', t.string_node(node.field_name))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'name_type', t.type_node(node.name_type))
	to_object(obj, 'from_embed_type', t.type_node(node.from_embed_type))
	to_object(obj, 'next_token', t.token_node(node.next_token))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) range_expr(node ast.RangeExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('RangeExpr'))
	to_object(obj, 'low', t.expr(node.low))
	to_object(obj, 'high', t.expr(node.high))
	to_object(obj, 'has_high', t.bool_node(node.has_high))
	to_object(obj, 'has_low', t.bool_node(node.has_low))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_expr(node ast.IfExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfExpr'))
	to_object(obj, 'is_comptime', t.bool_node(node.is_comptime))
	to_object(obj, 'tok_kind', t.token_node(node.tok_kind))
	branch_arr := create_array()
	for b in node.branches {
		to_array(branch_arr, t.if_branch(b))
	}
	to_object(obj, 'branches', branch_arr)
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'has_else', t.bool_node(node.has_else))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'pos', t.position(node.pos))
	comment_array := create_array()
	for c in node.post_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'post_comments', comment_array)
	return obj
}

fn (t Tree) if_branch(node ast.IfBranch) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfBranch'))
	to_object(obj, 'cond', t.expr(node.cond))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'body_pos', t.position(node.body_pos))
	to_object(obj, 'smartcast', t.bool_node(node.smartcast))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	stmt_array := create_array()
	for s in node.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	return obj
}

fn (t Tree) ident(node ast.Ident) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Ident'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'language', t.enum_node(node.language))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'tok_kind', t.token_node(node.tok_kind))
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'info', t.ident_info(node.info))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'mut_pos', t.position(node.mut_pos))
	to_object(obj, 'obj', t.scope_object(node.obj))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
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
	to_object(obj, 'share', t.enum_node(node.share))
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
	to_object(obj, 'left', t.expr(node.left))
	to_object(obj, 'is_method', t.bool_node(node.is_method))
	to_object(obj, 'mod', t.string_node(node.mod))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'language', t.enum_node(node.language))
	to_object(obj, 'scope', t.number_node(int(node.scope)))
	arg_arr := create_array()
	for e in node.args {
		to_array(arg_arr, t.call_arg(e))
	}
	to_object(obj, 'args', arg_arr)

	t_arr := create_array()
	for e in node.expected_arg_types {
		to_array(t_arr, t.type_node(e))
	}
	generic_array := create_array()
	for g in node.generic_types {
		to_array(generic_array, t.type_node(g))
	}
	to_object(obj, 'generic_types', generic_array)

	to_object(obj, 'expected_arg_types', t_arr)
	to_object(obj, 'or_block', t.or_expr(node.or_block))
	to_object(obj, 'left_type', t.type_node(node.left_type))
	to_object(obj, 'receiver_type', t.type_node(node.receiver_type))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'should_be_skipped', t.bool_node(node.should_be_skipped))
	to_object(obj, 'generic_list_pos', t.position(node.generic_list_pos))
	to_object(obj, 'free_receiver', t.bool_node(node.free_receiver))
	to_object(obj, 'from_embed_type', t.type_node(node.from_embed_type))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) call_arg(node ast.CallArg) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('CallArg'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'share', t.enum_node(node.share))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_tmp_autofree', t.bool_node(node.is_tmp_autofree))
	to_object(obj, 'pos', t.position(node.pos))
	comments := create_array()
	for c in node.comments {
		to_array(comments, t.comment(c))
	}
	to_object(obj, 'comments', comments)
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
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_init(node ast.StructInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructInit'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'is_short', t.bool_node(node.is_short))
	to_object(obj, 'unresolved', t.bool_node(node.unresolved))
	to_object(obj, 'has_update_expr', t.bool_node(node.has_update_expr))
	to_object(obj, 'update_expr', t.expr(node.update_expr))
	to_object(obj, 'update_expr_type', t.type_node(node.update_expr_type))
	to_object(obj, 'pos', t.position(node.pos))

	update_comments := create_array()
	for c in node.update_expr_comments {
		to_array(update_comments, t.comment(c))
	}
	to_object(obj, 'update_expr_comments', update_comments)

	field_array := create_array()
	for f in node.fields {
		to_array(field_array, t.struct_init_field(f))
	}
	to_object(obj, 'fields', field_array)

	embed_array := create_array()
	for e in node.embeds {
		to_array(embed_array, t.struct_init_embed(e))
	}
	to_object(obj, 'embeds', embed_array)

	comments := create_array()
	for c in node.pre_comments {
		to_array(comments, t.comment(c))
	}
	to_object(obj, 'pre_comments', comments)
	
	return obj
}

fn (t Tree) struct_init_field(node ast.StructInitField) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructInitField'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'expected_type', t.type_node(node.expected_type))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	next_comment_array := create_array()
	for c in node.next_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'next_comments', next_comment_array)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_init_embed(node ast.StructInitEmbed) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('StructInitEmbed'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'expected_type', t.type_node(node.expected_type))
	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
	next_comment_array := create_array()
	for c in node.next_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'next_comments', next_comment_array)
	to_object(obj, 'pos', t.position(node.pos))
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
	// expr comments:[][]Comment
	expr_comments := create_array()
	for c_array in node.ecmnts {
		comment_array := create_array()
		for c in c_array {
			to_array(comment_array, t.comment(c))
		}
		to_array(expr_comments, comment_array)
	}
	to_object(obj, 'ecmnts', expr_comments)
	//
	pre_comment_array := create_array()
	for c in node.pre_cmnts {
		to_array(pre_comment_array, t.comment(c))
	}
	to_object(obj, 'pre_cmnts', pre_comment_array)

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
	expr_array := create_array()
	for e in node.expr_types {
		to_array(expr_array, t.type_node(e))
	}
	to_object(obj, 'expr_types', expr_array)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) map_init(node ast.MapInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('MapInit'))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'key_type', t.type_node(node.key_type))
	to_object(obj, 'value_type', t.type_node(node.value_type))
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

	// expr comments:[][]Comment
	comments := create_array()
	for c_array in node.comments {
		comment_array := create_array()
		for c in c_array {
			to_array(comment_array, t.comment(c))
		}
		to_array(comments, comment_array)
	}
	to_object(obj, 'comments', comments)
	//
	pre_comment_array := create_array()
	for c in node.pre_cmnts {
		to_array(pre_comment_array, t.comment(c))
	}
	to_object(obj, 'pre_cmnts', pre_comment_array)

	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) none_expr(node ast.None) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('None'))
	to_object(obj, 'foo', t.number_node(node.foo))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) par_expr(node ast.ParExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ParExpr'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_guard_expr(node ast.IfGuardExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('IfGuardExpr'))
	to_object(obj, 'var_name', t.string_node(node.var_name))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) match_expr(node ast.MatchExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('MatchExpr'))
	to_object(obj, 'tok_kind', t.token_node(node.tok_kind))
	to_object(obj, 'cond', t.expr(node.cond))
	to_object(obj, 'cond_type', t.type_node(node.cond_type))
	to_object(obj, 'return_type', t.type_node(node.return_type))
	to_object(obj, 'expected_type', t.type_node(node.expected_type))
	to_object(obj, 'is_sum_type', t.bool_node(node.is_sum_type))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'pos', t.position(node.pos))
	branch_array := create_array()
	for b in node.branches {
		to_array(branch_array, t.match_branch(b))
	}
	to_object(obj, 'branches', branch_array)

	comment_array := create_array()
	for c in node.comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'comments', comment_array)
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
	// expr comments:[][]Comment
	expr_comments := create_array()
	for c_array in node.ecmnts {
		comment_array := create_array()
		for c in c_array {
			to_array(comment_array, t.comment(c))
		}
		to_array(expr_comments, comment_array)
	}
	to_object(obj, 'ecmnts', expr_comments)
	stmt_arr := create_array()
	for s in node.stmts {
		to_array(stmt_arr, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_arr)
	to_object(obj, 'is_else', t.bool_node(node.is_else))
	to_object(obj, 'pos', t.position(node.pos))

	c_array := create_array()
	for c in node.post_comments {
		to_array(c_array, t.comment(c))
	}
	to_object(obj, 'post_comments', c_array)
	to_object(obj, 'scope', t.number_node(int(node.scope)))
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
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_of(node ast.TypeOf) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('TypeOf'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'expr_type', t.type_node(node.expr_type))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) likely(node ast.Likely) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('Likely'))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'is_likely', t.bool_node(node.is_likely))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) sql_expr(node ast.SqlExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SqlExpr'))
	to_object(obj, 'type', t.type_node(node.typ))
	to_object(obj, 'is_count', t.bool_node(node.is_count))
	to_object(obj, 'db_expr', t.expr(node.db_expr))
	to_object(obj, 'table_expr', t.type_expr(node.table_expr))
	to_object(obj, 'has_where', t.bool_node(node.has_where))
	to_object(obj, 'where_expr', t.expr(node.where_expr))
	to_object(obj, 'has_order', t.bool_node(node.has_order))
	to_object(obj, 'order_expr', t.expr(node.order_expr))
	to_object(obj, 'has_desc', t.bool_node(node.has_desc))
	to_object(obj, 'is_array', t.bool_node(node.is_array))
	to_object(obj, 'pos', t.position(node.pos))
	to_object(obj, 'has_limit', t.bool_node(node.has_limit))
	to_object(obj, 'limit_expr', t.expr(node.limit_expr))
	to_object(obj, 'has_offset', t.bool_node(node.has_offset))
	to_object(obj, 'offset_expr', t.expr(node.offset_expr))
	field_array := create_array()
	for f in node.fields {
		to_array(field_array, t.table_field(f))
	}
	to_object(obj, 'fields', field_array)
	sub_struct_map := create_object()
	for key, val in node.sub_structs {
		to_object(sub_struct_map, key.str(), t.sql_expr(val))
	}
	to_object(obj, 'sub_structs', sub_struct_map)
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
		to_array(arr, t.attr(a))
	}
	to_object(obj, 'attrs', arr)
	to_object(obj, 'is_pub', t.bool_node(node.is_pub))
	to_object(obj, 'is_mut', t.bool_node(node.is_mut))
	to_object(obj, 'is_global', t.bool_node(node.is_global))
	to_object(obj, 'typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) sql_stmt(node ast.SqlStmt) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SqlStmt'))
	to_object(obj, 'kind', t.enum_node(node.kind))
	to_object(obj, 'db_expr', t.expr(node.db_expr))
	to_object(obj, 'table_expr', t.type_expr(node.table_expr))
	to_object(obj, 'object_var_name', t.string_node(node.object_var_name))
	to_object(obj, 'where_expr', t.expr(node.where_expr))

	field_array := create_array()
	for f in node.fields {
		to_array(field_array, t.table_field(f))
	}
	to_object(obj, 'fields', field_array)

	column_array := create_array()
	for c in node.updated_columns {
		to_array(column_array, t.string_node(c))
	}
	to_object(obj, 'updated_columns', column_array)

	expr_array := create_array()
	for e in node.update_exprs {
		to_array(expr_array, t.expr(e))
	}
	to_object(obj, 'update_exprs', expr_array)

	sub_struct_map := create_object()
	for key, val in node.sub_structs {
		to_object(sub_struct_map, key.str(), t.sql_stmt(val))
	}
	to_object(obj, 'sub_structs', sub_struct_map)

	to_object(obj, 'pos', t.position(node.pos))

	return obj
}

fn (t Tree) lock_expr(node ast.LockExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('LockExpr'))
	to_object(obj, 'is_expr', t.bool_node(node.is_expr))
	to_object(obj, 'typ', t.type_node(node.typ))
	to_object(obj, 'pos', t.position(node.pos))

	stmt_array := create_array()
	for s in node.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)

	ident_array := create_array()
	for i in node.lockeds {
		to_array(ident_array, t.ident(i))
	}
	to_object(obj, 'lockeds', ident_array)

	rlock_array := create_array()
	for r in node.is_rlock {
		to_array(rlock_array, t.bool_node(r))
	}
	to_object(obj, 'r_lock', rlock_array)

	return obj
}

fn (t Tree) unsafe_expr(expr ast.UnsafeExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('UnsafeExpr'))
	to_object(obj, 'expr', t.expr(expr.expr))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) chan_init(expr ast.ChanInit) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ChanInit'))
	to_object(obj, 'has_cap', t.bool_node(expr.has_cap))
	to_object(obj, 'cap_expr', t.expr(expr.cap_expr))
	to_object(obj, 'typ', t.type_node(expr.typ))
	to_object(obj, 'elem_type', t.type_node(expr.elem_type))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_expr(expr ast.SelectExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SelectExpr'))
	branch_array := create_array()
	for b in expr.branches {
		to_array(branch_array, t.select_branch(b))
	}
	to_object(obj, 'branches', branch_array)
	to_object(obj, 'is_expr', t.bool_node(expr.is_expr))
	to_object(obj, 'has_exception', t.bool_node(expr.has_exception))
	to_object(obj, 'expected_type', t.type_node(expr.expected_type))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_branch(expr ast.SelectBranch) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('SelectBranch'))
	to_object(obj, 'stmt', t.stmt(expr.stmt))
	stmt_array := create_array()
	for s in expr.stmts {
		to_array(stmt_array, t.stmt(s))
	}
	to_object(obj, 'stmts', stmt_array)
	to_object(obj, 'pos', t.position(expr.pos))
	to_object(obj, 'comment', t.comment(expr.comment))
	to_object(obj, 'is_else', t.bool_node(expr.is_else))
	to_object(obj, 'is_timeout', t.bool_node(expr.is_timeout))
	comment_array := create_array()
	for c in expr.post_comments {
		to_array(comment_array, t.comment(c))
	}
	to_object(obj, 'post_comments', comment_array)
	return obj
}

fn (t Tree) array_decompose(expr ast.ArrayDecompose) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('ArrayDecompose'))
	to_object(obj, 'expr', t.expr(expr.expr))
	to_object(obj, 'expr_type', t.type_node(expr.expr_type))
	to_object(obj, 'arg_type', t.type_node(expr.arg_type))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) go_expr(expr ast.GoExpr) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('GoExpr'))
	to_object(obj, 'go_stmt', t.go_stmt(expr.go_stmt))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) offset_of(expr ast.OffsetOf) &C.cJSON {
	obj := create_object()
	to_object(obj, 'ast_type', t.string_node('OffsetOf'))
	to_object(obj, 'struct_type', t.type_node(expr.struct_type))
	to_object(obj, 'field', t.string_node('field'))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) dump_expr(expr ast.DumpExpr) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('DumpExpr'))
	to_object(obj, 'expr', t.expr(expr.expr))
	to_object(obj, 'expr_type', t.type_node(expr.expr_type))
	to_object(obj, 'pos', t.position(expr.pos))
	return obj
}

fn (t Tree) asm_stmt(node ast.AsmStmt) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmStmt'))
	to_object(obj, 'arch', t.enum_node(node.arch))
	to_object(obj, 'is_top_level', t.bool_node(node.is_top_level))
	to_object(obj, 'is_volatile', t.bool_node(node.is_volatile))
	to_object(obj, 'is_goto', t.bool_node(node.is_goto))
	to_object(obj, 'scope', t.scope(node.scope))
	// to_object(obj, 'scope', t.number_node(int(node.scope)))
	to_object(obj, 'pos', t.position(node.pos))

	clobbered_array:=create_array()
	for c in node.clobbered {
		to_array(clobbered_array,t.asm_clobbered(c))
	}
	to_object(obj,'clobbered',clobbered_array)

	template_array:=create_array()
	for template in node.templates {
		to_array(template_array,t.asm_template(template))
	}
	to_object(obj,'templates',template_array)

	output_array:=create_array()
	for o in node.output {
		to_array(output_array,t.asm_io(o))
	}
	to_object(obj,'output',output_array)

	input_array:=create_array()
	for i in node.input {
		to_array(input_array,t.asm_io(i))
	}
	to_object(obj,'input',input_array)

	global_array:=create_array()
	for g in node.global_labels {
		to_array(global_array,t.string_node(g))
	}
	to_object(obj,'global_labels',global_array)

	local_array:=create_array()
	for l in node.local_labels {
		to_array(local_array,t.string_node(l))
	}
	to_object(obj,'local_labels',local_array)

	symbol_array:=create_array()
	for s in node.exported_symbols {
		to_array(symbol_array,t.string_node(s))
	}
	to_object(obj,'exported_symbols',symbol_array)

	return obj
}

fn (t Tree) asm_register(node ast.AsmRegister) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmRegister'))
	to_object(obj, 'name', t.string_node(node.name))
	// to_object(obj, 'typ', t.type_node(node.typ))
	// to_object(obj, 'size', t.number_node(node.size))
	return obj
}

fn (t Tree) asm_template(node ast.AsmTemplate) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmTemplate'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'is_label', t.bool_node(node.is_label))
	to_object(obj, 'is_directive', t.bool_node(node.is_directive))

	arg_array:=create_array()
	for arg in node.args {
		to_array(arg_array,t.asm_arg(arg))
	}
	to_object(obj,'args',arg_array)

	comment_array:=create_array()
	for c in node.comments {
		to_array(comment_array,t.comment(c))
	}
	to_object(obj,'comments',comment_array)
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_addressing(node ast.AsmAddressing) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmAddressing'))
	to_object(obj, 'displacement', t.number_node(int(node.displacement)))
	to_object(obj, 'scale', t.number_node(node.scale))
	to_object(obj, 'mode', t.enum_node(node.mode))
	to_object(obj, 'base', t.asm_arg(node.base))
	to_object(obj, 'index', t.asm_arg(node.index))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_arg(node ast.AsmArg) &C.cJSON {
	match node {
		ast.AsmAddressing {
			return t.asm_addressing(node)
		}
		ast.AsmAlias {
			return t.asm_alias(node)
		}
		ast.AsmRegister {
			return t.asm_register(node)
		}
		ast.BoolLiteral {
			return t.bool_literal(node)
		}
		ast.CharLiteral {
			return t.char_literal(node)
		}
		string {
			return t.string_node(node)
		}
		ast.FloatLiteral {
			return t.float_literal(node)
		}
		ast.IntegerLiteral {
			return t.integer_literal(node)
		}
	}
}

fn (t Tree) asm_alias(node ast.AsmAlias) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmAlias'))
	to_object(obj, 'name', t.string_node(node.name))
	to_object(obj, 'pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_clobbered(node ast.AsmClobbered) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmClobbered'))
	to_object(obj, 'reg', t.asm_register(node.reg))
	
	comment_array:=create_array()
	for c in node.comments {
		to_array(comment_array,t.comment(c))
	}
	to_object(obj,'comments',comment_array)
	return obj
}

fn (t Tree) asm_io(node ast.AsmIO) &C.cJSON {
	obj:=create_object()
	to_object(obj, 'ast_type', t.string_node('AsmIO'))
	to_object(obj, 'alias', t.string_node(node.alias))
	to_object(obj, 'constraint', t.string_node(node.constraint))
	to_object(obj, 'expr', t.expr(node.expr))
	to_object(obj, 'typ', t.type_node(node.typ))

	comment_array:=create_array()
	for c in node.comments {
		to_array(comment_array,t.comment(c))
	}
	to_object(obj,'comments',comment_array)

	to_object(obj, 'pos', t.position(node.pos))
	return obj
}


[inline]
fn to_object(node &C.cJSON, key string, child &C.cJSON) {
	add_item_to_object(node, key, child)
}

[inline]
fn to_array(node &C.cJSON, child &C.cJSON) {
	add_item_to_array(node, child)
}
