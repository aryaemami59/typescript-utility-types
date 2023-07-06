import {
  expectAssignable,
  expectNotAssignable,
  expectNotType,
  expectType,
  printType,
} from "tsd";

import type {
  AnyImmutableArray,
  AnyMutableArray,
  AnyObject,
  AssertInteger,
  AssertPositive,
  EmptyImmutableArray,
  EmptyImmutableTuple,
  EmptyMutableArray,
  EmptyMutableTuple,
  EmptyObject,
  Equal,
  ExclusiveRange,
  ExtendsFrom,
  FixedLengthTuple,
  IndexOf,
  Length,
  TupleOfRangedLength,
  TupleUnion,
  XOR,
} from ".";

// TupleUnion
expectAssignable<TupleUnion<["a", 2]>>(["a", 2]);
expectAssignable<TupleUnion<["a", 2]>>(["a", 2]);
expectAssignable<TupleUnion<["a", 2]>>([2, "a"]);
expectAssignable<TupleUnion<["a", 2]>>(["a", "a"]);
expectAssignable<TupleUnion<["a", 2]>>([2, 2]);
expectAssignable<TupleUnion<readonly ["a", 2]>>([2, 2] as const);
expectAssignable<TupleUnion<readonly ["a", 2]>>([2, 2]);
expectNotType<TupleUnion<["a", 2]>>([2, 2, 2]);
expectNotType<TupleUnion<["a", 2]>>([2]);
expectNotType<TupleUnion<["a", 2]>>([2, 2] as const);
// Tuple
expectAssignable<FixedLengthTuple<string, 1 | 2>>(["a"]);
expectAssignable<FixedLengthTuple<string, 1 | 2>>(["a", "b"]);
expectAssignable<FixedLengthTuple<string, 1 | 2, true>>(["a", "b"]);
expectAssignable<FixedLengthTuple<string, 1 | 2, true>>(["a", "b"] as const);
expectNotType<FixedLengthTuple<string, 1 | 2>>(["a", "b"] as const);
// TupleOfRangedLength
expectAssignable<TupleOfRangedLength<"a" | "b", 1, 3>>(["a"]);
// Length
expectType<Length<["a"]>>(1);
expectType<Length<[]>>(0);
// Equals
expectType<Equal<EmptyImmutableTuple, FixedLengthTuple<never, 0, true>>>(true);
expectType<Equal<EmptyImmutableTuple, FixedLengthTuple<unknown, 0, true>>>(
  true
);
expectType<Equal<EmptyMutableTuple, FixedLengthTuple<never, 0>>>(true);
expectType<Equal<EmptyMutableTuple, FixedLengthTuple<unknown, 0>>>(true);
// ExtendsFrom
expectType<ExtendsFrom<EmptyImmutableTuple, EmptyImmutableArray>>(true);
expectType<ExtendsFrom<EmptyMutableTuple, EmptyMutableArray>>(true);
expectType<ExtendsFrom<EmptyImmutableArray, AnyImmutableArray>>(true);
expectType<ExtendsFrom<EmptyMutableArray, AnyMutableArray>>(true);
expectType<ExtendsFrom<EmptyObject, AnyObject>>(true);
expectType<ExtendsFrom<AnyMutableArray, AnyImmutableArray>>(true);
// Range
expectAssignable<ExclusiveRange<3>>(2);
expectAssignable<ExclusiveRange<3, 1>>(2);
expectAssignable<ExclusiveRange<999, 1>>(2);
expectNotType<ExclusiveRange<3, -1>>(2);
expectNotType<ExclusiveRange<3, 4>>(2);
declare const a: ExclusiveRange<3, 1>;
printType(a);
// TupleOfRangedLength
expectAssignable<TupleOfRangedLength<"a", 1, 3>>(["a"]);
expectAssignable<TupleOfRangedLength<"a", 1, 3>>(["a", "a"]);
declare const b: TupleOfRangedLength<"a" | "b", 1, 3>;
printType(b);
expectAssignable<TupleOfRangedLength<"a" | "b", 1, 3>>(["a", "b"]);
expectNotType<TupleOfRangedLength<"a", 1, 3>>(["a", "a", "a"]);
// IndexOf
expectAssignable<IndexOf<["a", "b", "c"]>>(1);
expectAssignable<IndexOf<readonly ["a", "b", "c"]>>(2);
// XOR
expectAssignable<XOR<{ a: "a" }, { b: "b" }>>({ a: "a" });
expectAssignable<XOR<{ a: "a" }, { b: "b" }>>({ b: "b" });
expectNotType<XOR<{ a: "a" }, { b: "b" }>>({ b: "b", a: "a" });
// AssertPositive
expectType<AssertPositive<1>>(1);
expectAssignable<AssertPositive<1 | 2>>(1);
expectNotType<AssertPositive<number>>(1);
expectNotType<AssertPositive<-1>>(1);
// AssertInteger
expectType<AssertInteger<1>>(1);
expectType<AssertInteger<-1>>(-1);
expectAssignable<AssertInteger<1 | 2>>(1);
expectNotType<AssertInteger<1.1>>(1);
expectNotAssignable<AssertInteger<1.1 | 2.2>>(1);
