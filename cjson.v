module main

import json

struct UseJson {
	x int
}

fn suppress_json_warning() {
	json.encode(UseJson{})
}

// struct C.cJSON {}
fn C.cJSON_CreateObject() &C.cJSON

fn C.cJSON_CreateArray() &C.cJSON

// fn C.cJSON_CreateBool(bool) &C.cJSON
fn C.cJSON_CreateTrue() &C.cJSON

fn C.cJSON_CreateFalse() &C.cJSON

fn C.cJSON_CreateNull() &C.cJSON

// fn C.cJSON_CreateNumber() &C.cJSON
// fn C.cJSON_CreateString() &C.cJSON
fn C.cJSON_CreateRaw(arg_1 byteptr) &C.cJSON

fn C.cJSON_IsInvalid(arg_1 voidptr) bool

fn C.cJSON_IsFalse(arg_1 voidptr) bool

// fn C.cJSON_IsTrue(voidptr) bool
fn C.cJSON_IsBool(arg_1 voidptr) bool

fn C.cJSON_IsNull(arg_1 voidptr) bool

fn C.cJSON_IsNumber(arg_1 voidptr) bool

fn C.cJSON_IsString(arg_1 voidptr) bool

fn C.cJSON_IsArray(arg_1 voidptr) bool

fn C.cJSON_IsObject(arg_1 voidptr) bool

fn C.cJSON_IsRaw(arg_1 voidptr) bool

fn C.cJSON_AddItemToObject(arg_1 voidptr, arg_2 byteptr, arg_3 voidptr)

fn C.cJSON_AddItemToArray(arg_1, arg_2 voidptr)

fn C.cJSON_Delete(arg_1 voidptr)

// fn C.cJSON_Parse() &C.cJSON
fn C.cJSON_Print() byteptr

// fn C.cJSON_PrintUnformatted() byteptr
// [inline]
fn create_object() &C.cJSON {
	return C.cJSON_CreateObject()
}

[inline]
fn create_array() &C.cJSON {
	return C.cJSON_CreateArray()
}

[inline]
fn create_string(val string) &C.cJSON {
	return C.cJSON_CreateString(val.str)
}

[inline]
fn create_number(val f64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[inline]
fn create_bool(val bool) &C.cJSON {
	return C.cJSON_CreateBool(val)
}

[inline]
fn create_true() &C.cJSON {
	return C.cJSON_CreateTrue()
}

[inline]
fn create_false() &C.cJSON {
	return C.cJSON_CreateFalse()
}

[inline]
fn create_null() &C.cJSON {
	return C.cJSON_CreateNull()
}

[inline]
fn delete(b voidptr) {
	C.cJSON_Delete(b)
}

[inline]
fn add_item_to_object(obj &C.cJSON, key string, item &C.cJSON) {
	C.cJSON_AddItemToObject(obj, key.str, item)
}

[inline]
fn add_item_to_array(obj, item &C.cJSON) {
	C.cJSON_AddItemToArray(obj, item)
}

fn json_print(json &C.cJSON) string {
	s := C.cJSON_Print(json)
	return tos(s, C.strlen(s))
}
