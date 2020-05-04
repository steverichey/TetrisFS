// Primitives.fst
// Copyright 2019 Steve Richey
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module Primitives

open FStar.List

// an array of exactly four integers
// valid - let foo: int4 = [0; 1; 2; 3]
// invalid - let bar: int4 = [0; 2]
type int4 = l: list int {length l == 4}

// a 2D array of four arrays of four integers each
// valid -
// let bar: int4x4 = [
//  [0; 1; 2; 3];
//  [0; 1; 2; 3];
//  [0; 1; 2; 3];
//  [0; 1; 2; 3];
// ]
// invalid - let foo: int4x4 = [[0; 1]]
type int4x4 = l: list int4 {length l == 4}

// an integer of either one or zero
// incredibly, \/ is logical or: https://github.com/FStarLang/FStar/wiki/F%2A-symbols-reference
type binary = i: int {i == 0 \/ i == 1}

type binary4 = l: list binary {length l == 4}
type binary4x4 = l: list binary4 {length l == 4}

type uint = i: int {i >= 0 \/ i < 255}

let binary4_index_max: uint = 5
type binary4_index = i: uint {i < binary4_index_max}

val nth: binary4 -> binary4_index -> binary

let rec nth list index =
  match index with
  | 0 -> hd list
  | _ -> nth list (index - 1)

let binary4_zero: binary4 = [0; 0; 0; 0]
let binary4_one: binary4 = [1; 1; 1; 1]

(* let rotate(bb: binary4x4): binary4x4 =
  match bb with
  | _ -> [nth bb 0; binary4_zero; binary4_zero; binary4_zero] *)

let foo: binary4 = [0; 1; 0; 1]
let bar: binary = nth foo 2
