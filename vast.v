module main

import v.token
import v.parser
import v.ast
import v.pref
import v.errors
import os
import time

// the version of vast will follow vlang
const version = '0.2.2'

const usage = '
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
		time.sleep(500 * time.millisecond)
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
	table        &ast.Table
	pref         &pref.Preferences
	global_scope &ast.Scope
mut:
	root Node // the root of tree
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
		root: new_object()
		table: ast.new_table()
		pref: new_preferences()
		global_scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
	}
	// parse file with comment
	ast_file := parser.parse_file(file, t.table, .parse_comments, t.pref, t.global_scope)
	t.root = t.ast_file(ast_file)
	// generate the ast string
	s := json_print(t.root)
	return s
}

// tree node
pub type Node = C.cJSON

[inline]
fn (node &Node) add(key string, child &Node) {
	add_item_to_object(node, key, child)
}

[inline]
fn (node &Node) add_item(child &Node) {
	add_item_to_array(node, child)
}

[inline]
fn new_object() &Node {
	return C.cJSON_CreateObject()
}

[inline]
fn new_array() &Node {
	return C.cJSON_CreateArray()
}

// string type node
fn (t Tree) string_node(val string) &Node {
	return create_string(val)
}

// number type node
fn (t Tree) number_node(val int) &Node {
	return create_number(val)
}

// bool type node
fn (t Tree) bool_node(val bool) &Node {
	if val {
		return create_true()
	} else {
		return create_false()
	}
}

// null type node
fn (t Tree) null_node() &Node {
	return create_null()
}

// type node
fn (t Tree) type_node(typ ast.Type) &Node {
	if typ == 0 {
		return create_null()
	} else {
		type_name := t.table.get_type_name(typ)
		return create_string(type_name)
	}
}

// token type node
fn (t Tree) token_node(tok_kind token.Kind) &Node {
	return t.string_node('token:${int(tok_kind)}($tok_kind.str())')
}

// enum type node
fn (t Tree) enum_node<T>(value T) &Node {
	return t.string_node('enum:${int(value)}($value)')
}

// for [][]comment
fn (t Tree) two_dimension_comment(node [][]ast.Comment) &Node {
	comments := new_array()
	for n in node {
		comment_array := new_array()
		for c in n {
			comment_array.add_item(t.comment(c))
		}
		comments.add_item(comment_array)
	}
	return comments
}

// ast file root node
fn (t Tree) ast_file(node ast.File) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ast.File'))
	obj.add('path', t.string_node(node.path))
	obj.add('path_base', t.string_node(node.path_base))
	obj.add('lines', t.number_node(node.nr_lines))
	obj.add('bytes', t.number_node(node.nr_bytes))
	obj.add('mod', t.mod(node.mod))
	obj.add('imports', t.imports(node.imports))
	obj.add('global_scope', t.scope(node.global_scope))
	obj.add('scope', t.scope(node.scope))
	obj.add('errors', t.errors(node.errors))
	obj.add('warnings', t.warnings(node.warnings))
	obj.add('notices', t.notices(node.notices))
	obj.add('auto_imports', t.array_node_string(node.auto_imports))

	symbol_obj := new_object()
	for key, val in node.imported_symbols {
		symbol_obj.add(key, t.string_node(val))
	}
	obj.add('imported_symbols', symbol_obj)

	obj.add('generic_fns', t.array_node_generic_fns(node.generic_fns))
	obj.add('embedded_files', t.array_node_embed_file(node.embedded_files))
	obj.add('global_labels', t.array_node_string(node.global_labels))
	obj.add('is_test', t.bool_node(node.is_test))
	obj.add('stmts', t.stmts(node.stmts))
	return obj
}

// embed files
fn (t Tree) embed_file(node ast.EmbeddedFile) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EmbeddedFile'))
	obj.add('rpath', t.string_node(node.rpath))
	obj.add('apath', t.string_node(node.apath))
	return obj
}

// ast module node
fn (t Tree) mod(node ast.Module) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Module'))
	obj.add('name', t.string_node(node.name))
	obj.add('short_name', t.string_node(node.short_name))
	obj.add('is_skipped', t.bool_node(node.is_skipped))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) scope(scope ast.Scope) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Scope'))
	obj.add('parent', t.string_node(ptr_str(scope.parent)))
	children_arr := new_array()
	for s in scope.children {
		mut children_obj := new_object()
		children_obj.add('parent', t.string_node(ptr_str(s.parent)))
		children_obj.add('start_pos', t.number_node(s.start_pos))
		children_obj.add('end_pos', t.number_node(s.end_pos))
		children_arr.add_item(children_obj)
	}
	obj.add('children', children_arr)
	obj.add('start_pos', t.number_node(scope.start_pos))
	obj.add('end_pos', t.number_node(scope.end_pos))
	obj.add('objects', t.objects(scope.objects))
	obj.add('struct_fields', t.array_node_scope_struct_field(scope.struct_fields))
	return obj
}

fn (t Tree) scope_struct_field(node ast.ScopeStructField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ScopeStructField'))
	obj.add('struct_type', t.type_node(node.struct_type))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('orig_type', t.type_node(node.orig_type))
	obj.add('pos', t.position(node.pos))
	obj.add('smartcasts', t.array_node_type(node.smartcasts))
	return obj
}

fn (t Tree) objects(so map[string]ast.ScopeObject) &Node {
	obj := new_object()
	for key, val in so {
		obj.add(key, t.scope_object(val))
	}
	return obj
}

fn (t Tree) scope_object(node ast.ScopeObject) &Node {
	obj := new_object()
	match node {
		ast.ConstField { t.const_field(node) }
		ast.GlobalField { t.global_field(node) }
		ast.Var { t.var(node) }
		ast.AsmRegister { t.asm_register(node) }
	}
	return obj
}

fn (t Tree) imports(imports []ast.Import) &Node {
	import_array := new_array()
	for imp in imports {
		import_array.add_item(t.import_module(imp))
	}
	return import_array
}

fn (t Tree) errors(errors []errors.Error) &Node {
	errs := new_array()
	for e in errors {
		obj := new_object()
		obj.add('message', t.string_node(e.message))
		obj.add('file_path', t.string_node(e.file_path))
		obj.add('pos', t.position(e.pos))
		obj.add('backtrace', t.string_node(e.backtrace))
		obj.add('reporter', t.enum_node(e.reporter))
		errs.add_item(obj)
	}
	return errs
}

fn (t Tree) warnings(warnings []errors.Warning) &Node {
	warns := new_array()
	for w in warnings {
		obj := new_object()
		obj.add('message', t.string_node(w.message))
		obj.add('file_path', t.string_node(w.file_path))
		obj.add('pos', t.position(w.pos))
		obj.add('reporter', t.enum_node(w.reporter))
		warns.add_item(obj)
	}
	return warns
}

fn (t Tree) notices(notices []errors.Notice) &Node {
	notice_array := new_array()
	for n in notices {
		obj := new_object()
		obj.add('message', t.string_node(n.message))
		obj.add('file_path', t.string_node(n.file_path))
		obj.add('pos', t.position(n.pos))
		obj.add('reporter', t.enum_node(n.reporter))
		notice_array.add_item(obj)
	}
	return notice_array
}

// stmt node
fn (t Tree) stmts(stmts []ast.Stmt) &Node {
	stmt_array := new_array()
	for s in stmts {
		stmt_array.add_item(t.stmt(s))
	}
	return stmt_array
}

fn (t Tree) stmt(node ast.Stmt) &Node {
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
		ast.Block { return t.block(node) }
		ast.SqlStmt { return t.sql_stmt(node) }
		ast.AsmStmt { return t.asm_stmt(node) }
		ast.NodeError { return t.node_error(node) }
		ast.EmptyStmt { return t.empty_stmt(node) }
	}
	// fixed ForCStmt without init stmt
	return t.null_node()
}

fn (t Tree) import_module(node ast.Import) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Import'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('alias', t.string_node(node.alias))
	obj.add('syms', t.array_node_import_symbol(node.syms))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	obj.add('mod_pos', t.position(node.mod_pos))
	obj.add('alias_pos', t.position(node.alias_pos))
	obj.add('syms_pos', t.position(node.syms_pos))
	return obj
}

fn (t Tree) import_symbol(node ast.ImportSymbol) &Node {
	obj := new_object()
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) position(p token.Position) &Node {
	obj := new_object()
	obj.add('line_nr', t.number_node(p.line_nr))
	obj.add('pos', t.number_node(p.pos))
	obj.add('len', t.number_node(p.len))
	return obj
}

fn (t Tree) comment(node ast.Comment) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Comment'))
	obj.add('text', t.string_node(node.text))
	obj.add('is_multi', t.bool_node(node.is_multi))
	obj.add('line_nr', t.number_node(node.line_nr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_decl(node ast.ConstDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ConstDecl'))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_block', t.bool_node(node.is_block))
	obj.add('fields', t.array_node_const_field(node.fields))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_field(node ast.ConstField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ConstField'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('typ', t.type_node(node.typ))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

// function declaration
fn (t Tree) fn_decl(node ast.FnDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('FnDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('mod', t.string_node(node.mod))
	obj.add('is_deprecated', t.bool_node(node.is_deprecated))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_variadic', t.bool_node(node.is_variadic))
	obj.add('is_anon', t.bool_node(node.is_anon))
	obj.add('is_manualfree', t.bool_node(node.is_manualfree))
	obj.add('is_main', t.bool_node(node.is_main))
	obj.add('is_test', t.bool_node(node.is_test))
	obj.add('is_conditional', t.bool_node(node.is_conditional))
	obj.add('is_keep_alive', t.bool_node(node.is_keep_alive))
	obj.add('receiver', t.struct_field(node.receiver))
	obj.add('receiver_pos', t.position(node.receiver_pos))
	obj.add('is_method', t.bool_node(node.is_method))
	obj.add('method_idx', t.number_node(node.method_idx))
	obj.add('rec_mut', t.bool_node(node.rec_mut))
	obj.add('rec_share', t.enum_node(node.rec_share))
	obj.add('language', t.enum_node(node.language))
	obj.add('no_body', t.bool_node(node.no_body))
	obj.add('is_builtin', t.bool_node(node.is_builtin))
	obj.add('is_direct_arr', t.bool_node(node.is_direct_arr))
	obj.add('pos', t.position(node.pos))
	obj.add('body_pos', t.position(node.body_pos))
	obj.add('return_type_pos', t.position(node.return_type_pos))
	obj.add('file', t.string_node(node.file))
	obj.add('has_return', t.bool_node(node.has_return))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('source_file', t.number_node(int(node.source_file)))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('skip_gen', t.bool_node(node.skip_gen))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('params', t.array_node_arg(node.params))
	obj.add('generic_names', t.array_node_string(node.generic_names))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('label_names', t.array_node_string(node.label_names))
	obj.add('defer_stmts', t.array_node_defer_stmt(node.defer_stmts))
	return obj
}

fn (t Tree) anon_fn(node ast.AnonFn) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AnonFn'))
	obj.add('decl', t.fn_decl(node.decl))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_gen', t.bool_node(node.has_gen))
	return obj
}

fn (t Tree) struct_decl(node ast.StructDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StructDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('pub_pos', t.number_node(node.pub_pos))
	obj.add('mut_pos', t.number_node(node.mut_pos))
	obj.add('pub_mut_pos', t.number_node(node.pub_mut_pos))
	obj.add('global_pos', t.number_node(node.global_pos))
	obj.add('module_pos', t.number_node(node.module_pos))
	obj.add('language', t.enum_node(node.language))
	obj.add('is_union', t.bool_node(node.is_union))
	obj.add('pos', t.position(node.pos))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('generic_types', t.array_node_type(node.generic_types))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	obj.add('embeds', t.array_node_embed(node.embeds))
	return obj
}

fn (t Tree) struct_field(node ast.StructField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StructField'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('type_pos', t.position(node.type_pos))
	obj.add('has_default_expr', t.bool_node(node.has_default_expr))
	obj.add('default_expr_typ', t.type_node(node.default_expr_typ))
	obj.add('default_expr', t.expr(node.default_expr))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_global', t.bool_node(node.is_global))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) embed(node ast.Embed) &Node {
	obj := new_object()
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) enum_decl(node ast.EnumDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EnumDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_flag', t.bool_node(node.is_flag))
	obj.add('is_multi_allowed', t.bool_node(node.is_multi_allowed))
	obj.add('pos', t.position(node.pos))
	obj.add('fields', t.array_node_enum_field(node.fields))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('attrs', t.array_node_attr(node.attrs))
	return obj
}

fn (t Tree) enum_field(node ast.EnumField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EnumField'))
	obj.add('name', t.string_node(node.name))
	obj.add('has_expr', t.bool_node(node.has_expr))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	return obj
}

fn (t Tree) interface_decl(node ast.InterfaceDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('InterfaceDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('mut_pos', t.number_node(node.mut_pos))
	obj.add('field_names', t.array_node_string(node.field_names))
	obj.add('methods', t.array_node_fn_decl(node.methods))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('pre_comments', t.array_node_comment(node.pre_comments))
	obj.add('name_pos', t.position(node.name_pos))
	obj.add('language', t.enum_node(node.language))
	obj.add('pos', t.position(node.pos))
	obj.add('are_ifaces_expanded', t.bool_node(node.are_ifaces_expanded))
	obj.add('ifaces', t.array_node_interface_embedding(node.ifaces))
	return obj
}

fn (t Tree) interface_embedding(node ast.InterfaceEmbedding) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('InterfaceEmbedding'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) attr(node ast.Attr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Attr'))
	obj.add('name', t.string_node(node.name))
	obj.add('has_arg', t.bool_node(node.has_arg))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('arg', t.string_node(node.arg))
	return obj
}

fn (t Tree) hash_stmt(node ast.HashStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('HashStmt'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('val', t.string_node(node.val))
	obj.add('kind', t.string_node(node.kind))
	obj.add('main', t.string_node(node.main))
	obj.add('msg', t.string_node(node.msg))
	obj.add('source_file', t.string_node(node.source_file))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) comp_for(node ast.CompFor) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('CompFor'))
	obj.add('val_var', t.string_node(node.val_var))
	obj.add('typ', t.type_node(node.typ))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('pos', t.position(node.pos))
	obj.add('typ_pos', t.position(node.pos))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) global_decl(node ast.GlobalDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('GlobalDecl'))
	obj.add('pos', t.position(node.pos))
	obj.add('is_block', t.bool_node(node.is_block))
	obj.add('fields', t.array_node_global_field(node.fields))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	return obj
}

fn (t Tree) global_field(node ast.GlobalField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('GlobalField'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_expr', t.bool_node(node.has_expr))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	obj.add('typ_pos', t.position(node.typ_pos))
	return obj
}

fn (t Tree) defer_stmt(node ast.DeferStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('DeferStmt'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('ifdef', t.string_node(node.ifdef))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_decl(node ast.TypeDecl) &Node {
	match node {
		ast.AliasTypeDecl { return t.alias_type_decl(node) }
		ast.FnTypeDecl { return t.fn_type_decl(node) }
		ast.SumTypeDecl { return t.sum_type_decl(node) }
	}
}

fn (t Tree) alias_type_decl(node ast.AliasTypeDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AliasTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('parent_type', t.type_node(node.parent_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) sum_type_decl(node ast.SumTypeDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SumTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('pos', t.position(node.pos))
	obj.add('typ', t.type_node(node.typ))
	obj.add('comments', t.array_node_comment(node.comments))
	// variants
	t_array := new_array()
	for s in node.variants {
		variants_obj := new_object()
		variants_obj.add('typ', t.type_node(s.typ))
		variants_obj.add('pos', t.position(s.pos))
		t_array.add_item(variants_obj)
	}
	obj.add('variants', t_array)
	return obj
}

fn (t Tree) fn_type_decl(node ast.FnTypeDecl) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('FnTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) arg(node ast.Param) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Param'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	return obj
}

fn (t Tree) goto_label(node ast.GotoLabel) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('GotoLabel'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) goto_stmt(node ast.GotoStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('GotoStmt'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assign_stmt(node ast.AssignStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AssignStmt'))
	obj.add('left', t.array_node_expr(node.left))
	obj.add('left_types', t.array_node_type(node.left_types))
	obj.add('right', t.array_node_expr(node.right))
	obj.add('right_types', t.array_node_type(node.left_types))
	obj.add('op', t.token_node(node.op))
	obj.add('is_static', t.bool_node(node.is_static))
	obj.add('is_simple', t.bool_node(node.is_simple))
	obj.add('has_cross_var', t.bool_node(node.has_cross_var))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	return obj
}

fn (t Tree) var(node ast.Var) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Var'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('orig_type', t.type_node(node.orig_type))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_arg', t.bool_node(node.is_arg))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_used', t.bool_node(node.is_used))
	obj.add('is_changed', t.bool_node(node.is_changed))
	obj.add('is_or', t.bool_node(node.is_or))
	obj.add('is_tmp', t.bool_node(node.is_tmp))
	obj.add('is_autofree_tmp', t.bool_node(node.is_autofree_tmp))
	obj.add('is_auto_deref', t.bool_node(node.is_auto_deref))
	obj.add('is_auto_heap', t.bool_node(node.is_auto_heap))
	obj.add('is_stack_obj', t.bool_node(node.is_stack_obj))
	obj.add('share', t.enum_node(node.share))
	obj.add('pos', t.position(node.pos))
	obj.add('smartcasts', t.array_node_type(node.smartcasts))
	return obj
}

fn (t Tree) return_(node ast.Return) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Return'))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('types', t.array_node_type(node.types))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) for_c_stmt(node ast.ForCStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ForCStmt'))
	obj.add('init', t.stmt(node.init))
	obj.add('has_init', t.bool_node(node.has_init))
	obj.add('cond', t.expr(node.cond))
	obj.add('has_cond', t.bool_node(node.has_cond))
	obj.add('inc', t.stmt(node.inc))
	obj.add('has_inc', t.bool_node(node.has_inc))
	obj.add('is_multi', t.bool_node(node.is_multi))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) for_stmt(node ast.ForStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ForStmt'))
	obj.add('cond', t.expr(node.cond))
	obj.add('is_inf', t.bool_node(node.is_inf))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) for_in_stmt(node ast.ForInStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ForInStmt'))
	obj.add('key_var', t.string_node(node.key_var))
	obj.add('val_var', t.string_node(node.val_var))
	obj.add('cond', t.expr(node.cond))
	obj.add('is_range', t.bool_node(node.is_range))
	obj.add('high', t.expr(node.high))
	obj.add('key_type', t.type_node(node.key_type))
	obj.add('val_type', t.type_node(node.val_type))
	obj.add('cond_type', t.type_node(node.cond_type))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('val_is_mut', t.bool_node(node.val_is_mut))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) branch_stmt(node ast.BranchStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('BranchStmt'))
	obj.add('kind', t.token_node(node.kind))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assert_stmt(node ast.AssertStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AssertStmt'))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_used', t.bool_node(node.is_used))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) block(node ast.Block) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Block'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('is_unsafe', t.bool_node(node.is_unsafe))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) comptime_call(node ast.ComptimeCall) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ComptimeCall'))
	obj.add('method_name', t.string_node(node.method_name))
	obj.add('left', t.expr(node.left))
	obj.add('is_vweb', t.bool_node(node.is_vweb))
	obj.add('vweb_tmpl', t.string_node(node.vweb_tmpl.path))
	obj.add('args_var', t.string_node(node.args_var))
	obj.add('sym', t.string_node(node.sym.name))
	obj.add('has_parens', t.bool_node(node.has_parens))
	obj.add('is_embed', t.bool_node(node.is_embed))
	obj.add('embed_file', t.embed_file(node.embed_file))
	obj.add('method_pos', t.position(node.method_pos))
	obj.add('result_type', t.type_node(node.result_type))
	obj.add('scope', t.scope(node.scope))
	obj.add('env_value', t.string_node(node.env_value))
	obj.add('pos', t.position(node.pos))
	obj.add('args', t.array_node_call_arg(node.args))

	return obj
}

fn (t Tree) comptime_selector(node ast.ComptimeSelector) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ComptimeSelector'))
	obj.add('has_parens', t.bool_node(node.has_parens))
	obj.add('left', t.expr(node.left))
	obj.add('field_expr', t.expr(node.field_expr))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) expr_stmt(node ast.ExprStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ExprStmt'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

// expr
fn (t Tree) expr(expr ast.Expr) &Node {
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
		ast.TypeNode {
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
		ast.NodeError {
			return t.node_error(expr)
		}
		ast.EmptyExpr {
			return t.empty_expr(expr)
		}
		else {
			// println('unknown expr')
			return t.null_node()
		}
	}
}

fn (t Tree) integer_literal(node ast.IntegerLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IntegerLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) float_literal(node ast.FloatLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('FloatLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_literal(node ast.StringLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StringLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('is_raw', t.bool_node(node.is_raw))
	obj.add('language', t.enum_node(node.language))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) char_literal(node ast.CharLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('CharLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) bool_literal(node ast.BoolLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('BoolLiteral'))
	obj.add('val', t.bool_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_inter_literal(node ast.StringInterLiteral) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StringInterLiteral'))
	obj.add('vals', t.array_node_string(node.vals))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('expr_types', t.array_node_type(node.expr_types))
	obj.add('fwidths', t.array_node_int(node.fwidths))
	obj.add('precisions', t.array_node_int(node.precisions))
	obj.add('pluss', t.array_node_bool(node.pluss))
	obj.add('fills', t.array_node_bool(node.fills))
	obj.add('fmt_poss', t.array_node_position(node.fmt_poss))
	obj.add('fmts', t.array_node_byte(node.fmts))
	obj.add('need_fmts', t.array_node_bool(node.need_fmts))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) enum_val(node ast.EnumVal) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EnumVal'))
	obj.add('enum_name', t.string_node(node.enum_name))
	obj.add('mod', t.string_node(node.mod))
	obj.add('val', t.string_node(node.val))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assoc(node ast.Assoc) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Assoc'))
	obj.add('var_name', t.string_node(node.var_name))
	obj.add('fields', t.array_node_string(node.fields))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) at_expr(node ast.AtExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AtExpr'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('val', t.string_node(node.val))
	return obj
}

fn (t Tree) cast_expr(node ast.CastExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('CastExpr'))
	obj.add('expr', t.expr(node.expr))
	obj.add('arg', t.expr(node.arg))
	obj.add('typ', t.type_node(node.typ))
	obj.add('typname', t.string_node(node.typname))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('has_arg', t.bool_node(node.has_arg))
	obj.add('in_prexpr', t.bool_node(node.in_prexpr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) as_cast(node ast.AsCast) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsCast'))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_expr(node ast.TypeNode) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('TypeNode'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) size_of(node ast.SizeOf) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SizeOf'))
	obj.add('is_type', t.bool_node(node.is_type))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) prefix_expr(node ast.PrefixExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('PrefixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('right', t.expr(node.right))
	obj.add('right_type', t.type_node(node.right_type))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('is_option', t.bool_node(node.is_option))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) infix_expr(node ast.InfixExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('InfixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('left', t.expr(node.left))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('right', t.expr(node.right))
	obj.add('right_type', t.type_node(node.right_type))
	obj.add('auto_locked', t.string_node(node.auto_locked))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('is_stmt', t.bool_node(node.is_stmt))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) index_expr(node ast.IndexExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IndexExpr'))
	obj.add('left', t.expr(node.left))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('index', t.expr(node.index))
	obj.add('is_setter', t.bool_node(node.is_setter))
	obj.add('or_expr', t.or_expr(node.or_expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) postfix_expr(node ast.PostfixExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('PostfixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('expr', t.expr(node.expr))
	obj.add('auto_locked', t.string_node(node.auto_locked))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) selector_expr(node ast.SelectorExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SelectorExpr'))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('field_name', t.string_node(node.field_name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('name_type', t.type_node(node.name_type))
	obj.add('from_embed_type', t.type_node(node.from_embed_type))
	obj.add('next_token', t.token_node(node.next_token))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) range_expr(node ast.RangeExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('RangeExpr'))
	obj.add('low', t.expr(node.low))
	obj.add('high', t.expr(node.high))
	obj.add('has_high', t.bool_node(node.has_high))
	obj.add('has_low', t.bool_node(node.has_low))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_expr(node ast.IfExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IfExpr'))
	obj.add('is_comptime', t.bool_node(node.is_comptime))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('branches', t.array_node_if_branch(node.branches))
	obj.add('left', t.expr(node.left))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_else', t.bool_node(node.has_else))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('post_comments', t.array_node_comment(node.post_comments))
	return obj
}

fn (t Tree) if_branch(node ast.IfBranch) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IfBranch'))
	obj.add('cond', t.expr(node.cond))
	obj.add('pos', t.position(node.pos))
	obj.add('body_pos', t.position(node.body_pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) ident(node ast.Ident) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Ident'))
	obj.add('name', t.string_node(node.name))
	obj.add('mod', t.string_node(node.mod))
	obj.add('language', t.enum_node(node.language))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('info', t.ident_info(node.info))
	obj.add('pos', t.position(node.pos))
	obj.add('mut_pos', t.position(node.mut_pos))
	obj.add('obj', t.scope_object(node.obj))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) ident_info(info ast.IdentInfo) &Node {
	match info {
		ast.IdentVar { return t.ident_var(info) }
		ast.IdentFn { return t.ident_fn(info) }
	}
}

fn (t Tree) ident_var(node ast.IdentVar) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IdentVar'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_static', t.bool_node(node.is_static))
	obj.add('is_optional', t.bool_node(node.is_optional))
	obj.add('share', t.enum_node(node.share))
	return obj
}

fn (t Tree) ident_fn(node ast.IdentFn) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IdentFn'))
	obj.add('typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) call_expr(node ast.CallExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('CallExpr'))
	obj.add('left', t.expr(node.left))
	obj.add('is_method', t.bool_node(node.is_method))
	obj.add('mod', t.string_node(node.mod))
	obj.add('name', t.string_node(node.name))
	obj.add('language', t.enum_node(node.language))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('args', t.array_node_call_arg(node.args))
	obj.add('expected_arg_types', t.array_node_type(node.expected_arg_types))
	obj.add('concrete_types', t.array_node_type(node.concrete_types))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('receiver_type', t.type_node(node.receiver_type))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('should_be_skipped', t.bool_node(node.should_be_skipped))
	obj.add('concrete_list_pos', t.position(node.concrete_list_pos))
	obj.add('free_receiver', t.bool_node(node.free_receiver))
	obj.add('from_embed_type', t.type_node(node.from_embed_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	return obj
}

fn (t Tree) call_arg(node ast.CallArg) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('CallArg'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('share', t.enum_node(node.share))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_tmp_autofree', t.bool_node(node.is_tmp_autofree))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) or_expr(node ast.OrExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('OrExpr'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_init(node ast.StructInit) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StructInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_short', t.bool_node(node.is_short))
	obj.add('unresolved', t.bool_node(node.unresolved))
	obj.add('has_update_expr', t.bool_node(node.has_update_expr))
	obj.add('update_expr', t.expr(node.update_expr))
	obj.add('update_expr_type', t.type_node(node.update_expr_type))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	obj.add('update_expr_comments', t.array_node_comment(node.update_expr_comments))
	obj.add('fields', t.array_node_struct_init_field(node.fields))
	obj.add('embeds', t.array_node_struct_init_embed(node.embeds))
	obj.add('pre_comments', t.array_node_comment(node.pre_comments))
	return obj
}

fn (t Tree) struct_init_field(node ast.StructInitField) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StructInitField'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('parent_type', t.type_node(node.parent_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	return obj
}

fn (t Tree) struct_init_embed(node ast.StructInitEmbed) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('StructInitEmbed'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) array_init(node ast.ArrayInit) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ArrayInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('elem_type', t.type_node(node.elem_type))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('ecmnts', t.two_dimension_comment(node.ecmnts))
	obj.add('pre_cmnts', t.array_node_comment(node.pre_cmnts))
	obj.add('elem_type_pos', t.position(node.elem_type_pos))
	obj.add('is_fixed', t.bool_node(node.is_fixed))
	obj.add('has_val', t.bool_node(node.has_val))
	obj.add('mod', t.string_node(node.mod))
	obj.add('len_expr', t.expr(node.len_expr))
	obj.add('cap_expr', t.expr(node.cap_expr))
	obj.add('default_expr', t.expr(node.default_expr))
	obj.add('has_len', t.bool_node(node.has_len))
	obj.add('has_cap', t.bool_node(node.has_cap))
	obj.add('has_default', t.bool_node(node.has_default))
	obj.add('expr_types', t.array_node_type(node.expr_types))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) map_init(node ast.MapInit) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('MapInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('key_type', t.type_node(node.key_type))
	obj.add('value_type', t.type_node(node.value_type))
	obj.add('keys', t.array_node_expr(node.keys))
	obj.add('vals', t.array_node_expr(node.vals))
	obj.add('comments', t.two_dimension_comment(node.comments))
	obj.add('pre_cmnts', t.array_node_comment(node.pre_cmnts))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) none_expr(node ast.None) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('None'))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) par_expr(node ast.ParExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ParExpr'))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_guard_expr(node ast.IfGuardExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('IfGuardExpr'))
	obj.add('var_name', t.string_node(node.var_name))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) match_expr(node ast.MatchExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('MatchExpr'))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('cond', t.expr(node.cond))
	obj.add('cond_type', t.type_node(node.cond_type))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('is_sum_type', t.bool_node(node.is_sum_type))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('branches', t.array_node_match_branch(node.branches))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) match_branch(node ast.MatchBranch) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('MatchBranch'))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('ecmnts', t.two_dimension_comment(node.ecmnts))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('is_else', t.bool_node(node.is_else))
	obj.add('pos', t.position(node.pos))
	obj.add('post_comments', t.array_node_comment(node.post_comments))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) concat_expr(node ast.ConcatExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ConcatExpr'))
	obj.add('vals', t.array_node_expr(node.vals))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_of(node ast.TypeOf) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('TypeOf'))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) likely(node ast.Likely) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('Likely'))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_likely', t.bool_node(node.is_likely))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) sql_expr(node ast.SqlExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SqlExpr'))
	obj.add('type', t.type_node(node.typ))
	obj.add('is_count', t.bool_node(node.is_count))
	obj.add('db_expr', t.expr(node.db_expr))
	obj.add('table_expr', t.type_expr(node.table_expr))
	obj.add('has_where', t.bool_node(node.has_where))
	obj.add('where_expr', t.expr(node.where_expr))
	obj.add('has_order', t.bool_node(node.has_order))
	obj.add('order_expr', t.expr(node.order_expr))
	obj.add('has_desc', t.bool_node(node.has_desc))
	obj.add('is_array', t.bool_node(node.is_array))
	obj.add('pos', t.position(node.pos))
	obj.add('has_limit', t.bool_node(node.has_limit))
	obj.add('limit_expr', t.expr(node.limit_expr))
	obj.add('has_offset', t.bool_node(node.has_offset))
	obj.add('offset_expr', t.expr(node.offset_expr))
	obj.add('fields', t.array_node_struct_field(node.fields))
	sub_struct_map := new_object()
	for key, val in node.sub_structs {
		sub_struct_map.add(key.str(), t.sql_expr(val))
	}
	obj.add('sub_structs', sub_struct_map)
	return obj
}

fn (t Tree) sql_stmt(node ast.SqlStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SqlStmt'))
	obj.add('db_expr', t.expr(node.db_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('lines', t.array_node_sql_stmt_line(node.lines))
	return obj
}

fn (t Tree) sql_stmt_line(node ast.SqlStmtLine) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SqlStmtLine'))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('table_expr', t.type_expr(node.table_expr))
	obj.add('object_var_name', t.string_node(node.object_var_name))
	obj.add('where_expr', t.expr(node.where_expr))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('updated_columns', t.array_node_string(node.updated_columns))
	obj.add('update_exprs', t.array_node_expr(node.update_exprs))
	obj.add('pos', t.position(node.pos))

	sub_struct_map := new_object()
	for key, val in node.sub_structs {
		sub_struct_map.add(key.str(), t.sql_stmt_line(val))
	}
	obj.add('sub_structs', sub_struct_map)
	return obj
}

fn (t Tree) lock_expr(node ast.LockExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('LockExpr'))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('lockeds', t.array_node_expr(node.lockeds))
	obj.add('r_lock', t.array_node_bool(node.is_rlock))
	return obj
}

fn (t Tree) unsafe_expr(expr ast.UnsafeExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('UnsafeExpr'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) chan_init(expr ast.ChanInit) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ChanInit'))
	obj.add('has_cap', t.bool_node(expr.has_cap))
	obj.add('cap_expr', t.expr(expr.cap_expr))
	obj.add('typ', t.type_node(expr.typ))
	obj.add('elem_type', t.type_node(expr.elem_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_expr(expr ast.SelectExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SelectExpr'))
	obj.add('branches', t.array_node_select_branch(expr.branches))
	obj.add('is_expr', t.bool_node(expr.is_expr))
	obj.add('has_exception', t.bool_node(expr.has_exception))
	obj.add('expected_type', t.type_node(expr.expected_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_branch(expr ast.SelectBranch) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('SelectBranch'))
	obj.add('stmt', t.stmt(expr.stmt))
	obj.add('stmts', t.array_node_stmt(expr.stmts))
	obj.add('pos', t.position(expr.pos))
	obj.add('comment', t.comment(expr.comment))
	obj.add('is_else', t.bool_node(expr.is_else))
	obj.add('is_timeout', t.bool_node(expr.is_timeout))
	obj.add('post_comments', t.array_node_comment(expr.post_comments))
	return obj
}

fn (t Tree) array_decompose(expr ast.ArrayDecompose) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('ArrayDecompose'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('expr_type', t.type_node(expr.expr_type))
	obj.add('arg_type', t.type_node(expr.arg_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) go_expr(expr ast.GoExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('GoExpr'))
	obj.add('call_expr', t.call_expr(expr.call_expr))
	obj.add('is_expr', t.bool_node(expr.is_expr))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) offset_of(expr ast.OffsetOf) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('OffsetOf'))
	obj.add('struct_type', t.type_node(expr.struct_type))
	obj.add('field', t.string_node('field'))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) dump_expr(expr ast.DumpExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('DumpExpr'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('expr_type', t.type_node(expr.expr_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) node_error(expr ast.NodeError) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('NodeError'))
	obj.add('idx', t.number_node(expr.idx))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) empty_expr(expr ast.EmptyExpr) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EmptyExpr'))
	// obj.add('x', t.number_node(expr.x))
	return obj
}

fn (t Tree) empty_stmt(node ast.EmptyStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('EmptyStmt'))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_stmt(node ast.AsmStmt) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmStmt'))
	obj.add('arch', t.enum_node(node.arch))
	obj.add('is_top_level', t.bool_node(node.is_top_level))
	obj.add('is_volatile', t.bool_node(node.is_volatile))
	obj.add('is_goto', t.bool_node(node.is_goto))
	obj.add('scope', t.scope(node.scope))
	// obj.add('scope', t.number_node(int(node.scope)))
	obj.add('pos', t.position(node.pos))
	obj.add('clobbered', t.array_node_asm_clobbered(node.clobbered))
	obj.add('templates', t.array_node_asm_template(node.templates))
	obj.add('output', t.array_node_asm_io(node.output))
	obj.add('input', t.array_node_asm_io(node.input))
	obj.add('global_labels', t.array_node_string(node.global_labels))
	obj.add('local_labels', t.array_node_string(node.local_labels))
	return obj
}

fn (t Tree) asm_register(node ast.AsmRegister) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmRegister'))
	obj.add('name', t.string_node(node.name))
	// obj.add('typ', t.type_node(node.typ))
	// obj.add('size', t.number_node(node.size))
	return obj
}

fn (t Tree) asm_template(node ast.AsmTemplate) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmTemplate'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_label', t.bool_node(node.is_label))
	obj.add('is_directive', t.bool_node(node.is_directive))
	obj.add('args', t.array_node_asm_arg(node.args))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_addressing(node ast.AsmAddressing) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmAddressing'))
	obj.add('scale', t.number_node(node.scale))
	obj.add('mode', t.enum_node(node.mode))
	obj.add('displacement', t.asm_arg(node.displacement))
	obj.add('base', t.asm_arg(node.base))
	obj.add('index', t.asm_arg(node.index))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_arg(node ast.AsmArg) &Node {
	match node {
		ast.AsmAddressing {
			return t.asm_addressing(node)
		}
		ast.AsmAlias {
			return t.asm_alias(node)
		}
		ast.AsmDisp {
			return t.asm_disp(node)
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
		ast.FloatLiteral {
			return t.float_literal(node)
		}
		ast.IntegerLiteral {
			return t.integer_literal(node)
		}
		string {
			return t.string_node(node)
		}
	}
}

fn (t Tree) asm_alias(node ast.AsmAlias) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmAlias'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_disp(node ast.AsmDisp) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmDisp'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_clobbered(node ast.AsmClobbered) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmClobbered'))
	obj.add('reg', t.asm_register(node.reg))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) asm_io(node ast.AsmIO) &Node {
	obj := new_object()
	obj.add('ast_type', t.string_node('AsmIO'))
	obj.add('alias', t.string_node(node.alias))
	obj.add('constraint', t.string_node(node.constraint))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}
