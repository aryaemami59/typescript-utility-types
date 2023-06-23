export type AnyObject = Record<string, unknown>;
/** Does not have access to  mutable methods like `push` and `pop`. */
export type AnyTuple = readonly [...unknown[]];
/** extends from {@link AnyTuple} */
export type AnyArray = unknown[];
/** extends from {@link Function} */
export type AnyFunction = (...args: unknown[]) => unknown;
/** extends from {@link AnyObject} */
export type EmptyObject = Record<string, never>;
/** This array has access to mutable methods like `push` and `pop` but it cannot use them. */
export type MutableEmptyArray = never[];
/** This array does not have access to mutable methods like `push` and `pop`. */
export type ImmutableEmptyArray = readonly never[];

export type AnyNonNullishValue = NonNullable<unknown>;

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
/** Tuple of specific length. Set R to true to make it readonly. N cannot be a negative number. */
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

export type RecursiveImmutable<T> = {
  +readonly [P in keyof T]: RecursiveImmutable<T[P]>;
};

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
