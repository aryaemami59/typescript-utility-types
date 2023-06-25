export type AnyObject = Record<string, unknown>;
/** Does not have access to  mutable methods like `push` and `pop`. */
export type AnyTuple = readonly [...unknown[]];
/** extends from {@link AnyTuple} */
export type AnyArray = unknown[];
/** extends from {@link Function} */
export type AnyFunction = (...args: unknown[]) => unknown;

export type AnyNonNullishValue = NonNullable<unknown>;
/** Can be used to generate intellisense for types that are a union of some strings and any string */
export type AnyString = string & AnyNonNullishValue;

/** extends from {@link AnyObject} */
export type EmptyObject = Record<string, never>;
/** This array has access to mutable methods like `push` and `pop` but it cannot use them. */
export type MutableEmptyArray = never[];
/** This array does not have access to mutable methods like `push` and `pop`. */
export type ImmutableEmptyArray = readonly never[];

type Enumerate<
  N extends number,
  Acc extends number[] = []
> = N extends AssertPositive<N>
  ? Acc["length"] extends N
    ? Acc[number]
    : Enumerate<N, [Acc["length"], ...Acc]>
  : never;
/** Specify range of numbers starting from 0. T cannot be higher than 999. F is the starting value, it cannot be a negative number. */
export type Range<
  T extends number,
  F extends number = 0
> = F extends AssertPositive<F> ? Exclude<Enumerate<T>, Enumerate<F>> : never;
/** Checks if a number is positive. */
export type AssertPositive<N extends number> = number extends N
  ? N
  : `${N}` extends `-${string}`
  ? never
  : N;

type TupleOf<
  T,
  N extends number,
  U extends readonly unknown[],
  TReadonly extends boolean = false
> = U["length"] extends N
  ? U
  : TReadonly extends true
  ? Readonly<TupleOf<T, N, readonly [T, ...Readonly<U>], TReadonly>>
  : TupleOf<T, N, [T, ...U], TReadonly>;
/**
 * Tuple of specific length.
 * T can be a union type.
 * Set R to true to make it readonly.
 * N cannot be a negative number.
 */
export type Tuple<
  T,
  N extends number,
  TReadonly extends boolean = false
> = N extends AssertPositive<N>
  ? number extends N
    ? readonly T[]
    : TReadonly extends true
    ? Readonly<TupleOf<T, N, [], TReadonly>>
    : TupleOf<T, N, [], TReadonly>
  : never;

export type TupleUnion<TTuple extends AnyTuple> = TTuple extends [
  ...[...infer R]
]
  ? Tuple<R[IndexOf<R>], TTuple["length"]>
  : never;

export type TupleOfMinLength<
  T,
  TMin extends number
> = TMin extends AssertPositive<TMin> ? [...Tuple<T, TMin>, ...T[]] : never;

export type TupleOfMaxLength<
  T,
  TMax extends number
> = TMax extends AssertPositive<TMax> ? Partial<[...Tuple<T, TMax>]> : never;

export type TupleOfRangedLength<
  T,
  TMin extends number,
  TMax extends number
> = Range<TMax, TMin> extends number ? Tuple<T, Range<TMax, TMin>> : never;

// const element: TupleUnion<[Record<"a", unknown>, "a", "b"]> = ["a", "a", "b"];
const element: TupleOfRangedLength<Record<"a", unknown> | "a", 1, 4> = [
  "a",
  "a",
  "a",
  "a",
];

/** Checks if T extends from U. Checks if T is assignable to U. */
export type ExtendsFrom<T, U> = T extends U ? true : false;

export type Composite = AnyArray | AnyFunction | AnyObject;

export type ObjectOrArray = AnyArray | AnyObject;
/** Checks if 2 types are equal. */
export type Equals<X, Y> = (<T>() => T extends X ? 1 : 2) extends <
  T
>() => T extends Y ? 1 : 2
  ? true
  : false;

type Without<T, U> = { [P in Exclude<keyof T, keyof U>]?: never };
/** Exclusive OR */
export type XOR<T, U> = T | U extends object
  ? (T & Without<U, T>) | (U & Without<T, U>)
  : T | U;

export type RecursiveMutable<T> = {
  -readonly [P in keyof T]: RecursiveMutable<T[P]>;
};

export type Mutable<T> = { -readonly [P in keyof T]: T[P] };
/** Recursive {@link Readonly} */
export type RecursiveImmutable<T> = {
  +readonly [P in keyof T]: RecursiveImmutable<T[P]>;
};

export type RecursivePartial<T> = {
  [P in keyof T]?: RecursivePartial<T[P]>;
};

export type RecursiveRequired<T> = {
  [P in keyof T]-?: RecursiveRequired<T[P]>;
};

export type LowerOrUpperCase<T extends string> = Lowercase<T> | Uppercase<T>;

export type AnyCase<T extends string> = string extends T
  ? string
  : T extends `${infer F1}${infer F2}${infer R}`
  ? `${LowerOrUpperCase<F1>}${LowerOrUpperCase<F2>}${AnyCase<R>}`
  : T extends `${infer F}${infer R}`
  ? `${LowerOrUpperCase<F>}${AnyCase<R>}`
  : "";

export type IsReadOnly<T> = Readonly<T> extends T ? true : false;
/** Get a union type of the indices of an array. */
export type IndexOf<
  T extends AnyTuple,
  S extends readonly number[] = readonly []
> = T extends AnyArray
  ? never
  : T["length"] extends S["length"]
  ? S[number]
  : IndexOf<T, [S["length"], ...S]>;
