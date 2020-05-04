// Tetronimos.fst
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

module Tetrominos

open FStar.List
open Primitives

/// <summary>
/// A tetromino is a geometric shape composed of four squares, connected orthogonally.
/// https://en.wikipedia.org/wiki/Tetromino
/// </summary>
type tetromino =
  /// <summary>
  /// Four blocks in a straight line.
  /// </summary>
  | I
  /// <summary>
  /// Four blocks in a 2x2 square.
  /// </summary>
  | O
  /// <summary>
  /// A row of three blocks with one added below the center.
  /// </summary>
  | T
  /// <summary>
  /// A row of three blocks with one added below the right side.
  /// </summary>
  | J
  /// <summary>
  /// A row of three blocks with one added below the left side.
  /// </summary>
  | L
  /// <summary>
  /// Two stacked horizontal dominoes with the top one offset to the right.
  /// </summary>
  | S
  /// <summary>
  /// Two stacked horizontal dominoes with the top one offset to the left.
  /// </summary>
  | Z

/// <summary>
/// Return a 4x4 binary pattern describing the given tetromino.
/// </summary>
let getPattern(t: tetromino): binary4x4 =
  match t with
  | I -> [binary4_zero; binary4_zero; binary4_zero; binary4_one]
  | O -> [binary4_zero; binary4_zero; [1; 1; 0; 0]; [1; 1; 0; 0]]
  | T -> [binary4_zero; binary4_zero; [1; 1; 1; 0]; [0; 1; 0; 0]]
  | J -> [binary4_zero; binary4_zero; [1; 1; 1; 0]; [0; 0; 1; 0]]
  | L -> [binary4_zero; binary4_zero; [1; 1; 1; 0]; [1; 0; 0; 0]]
  | S -> [binary4_zero; binary4_zero; [0; 1; 1; 0]; [1; 1; 0; 0]]
  | Z -> [binary4_zero; binary4_zero; [1; 1; 0; 0]; [0; 1; 1; 0]]

/// <summary>
/// In geometry, a figure is chiral (and said to have chirality) if it is not identical to its mirror image, or, more precisely, if it cannot be mapped to its mirror image by rotations and translations alone.
/// https://en.wikipedia.org/wiki/Chirality_(mathematics)
/// </summary>
let isChiral(t: tetromino) =
  match t with
  | J | L | S | Z -> true
  | _ -> false

/// <summary>
/// Return the chiral version of a tetromino.
/// Non-chiral tetrominos will return themselves.
/// </summary>
let getChiral(t: tetromino) =
  match t with
  | J -> L
  | L -> J
  | S -> Z
  | Z -> S
  | _ -> t
