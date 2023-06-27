export type AnyObject = Record<string, unknown>;
/**
 * Does not have access to mutable methods like `push` and `pop`.
 *
 * same as {@link ReadonlyArray<unknown>}
 */
export type AnyImmutableArray = readonly unknown[];
/**
 * has access to mutable methods like `push` and `pop`.
 *
 * extends from {@link AnyImmutableArray}
 *
 * same as {@link Array<unknown>}
 */
export type AnyMutableArray = unknown[];
/**
 * extends from {@link Function}
 */
export type AnyFunction = (...args: unknown[]) => unknown;
/**
 * same as `{}`
 */
export type AnyNonNullishValue = NonNullable<unknown>;
/**
 * Can be used to generate intellisense for types that are a union of some strings and any string.
 *
 * @example
 * ```
 * const example: AnyString | "specific string" = "specific string";
 * const example: AnyString | "specific string" = "any other string";
 * It accepts any other string but also gives "specific string" in intellisense.
 * ```
 */
export type AnyString = string & AnyNonNullishValue;
/**
 * Can be used to generate intellisense for types that are a union of some numbers and any number.
 *
 * works similar to {@link AnyString}
 */
export type AnyNumber = number & AnyNonNullishValue;
/**
 * extends from {@link AnyObject}
 */
export type EmptyObject = Record<string, never>;
/**
 * This array has access to mutable methods like `push` and `pop` but it cannot use them.
 *
 * extends from {@link AnyMutableArray}
 */
export type EmptyMutableArray = never[];
/**
 * This array does not have access to mutable methods like `push` and `pop`.
 *
 * extends from {@link AnyImmutableArray}
 */
export type EmptyImmutableArray = readonly never[];
/**
 * extends from {@link EmptyMutableArray}
 *
 * same as
 * ```
 * Tuple<never, 0>
 * or:
 * Tuple<unknown, 0>
 * ```
 */
export type EmptyMutableTuple = [];
/**
 * extends from {@link EmptyImmutableArray}
 *
 * same as
 * ```
 * Tuple<never, 0, true>
 * or:
 * Tuple<unknown, 0, true>
 * ```
 */
export type EmptyImmutableTuple = readonly [];

type Enumerate<
  TNumber extends number,
  Acc extends number[] = []
> = TNumber extends AssertPositive<TNumber>
  ? Acc["length"] extends TNumber
    ? Acc[number]
    : Enumerate<TNumber, [Acc["length"], ...Acc]>
  : never;
/**
 * Specify exclusive range of numbers starting from 0.
 *
 * TMax cannot be higher than 999.
 *
 * TMin is the starting value, it cannot be a negative number.
 */
export type ExclusiveRange<
  TMax extends number,
  TMin extends number = 0
> = TMin extends AssertPositive<TMin>
  ? Exclude<Enumerate<TMax>, Enumerate<TMin>>
  : never;
/**
 * Checks if a number is positive. TNumber could be a union type but must be a literal number type.
 */
export type AssertPositive<TNumber extends number> = number extends TNumber
  ? never
  : `${TNumber}` extends `-${string}`
  ? never
  : TNumber;
/**
 * Checks if a number is an integer. TNumber could be a union type but must be a literal number type.
 */
export type AssertInteger<TNumber extends number> = number extends TNumber
  ? TNumber
  : `${TNumber}` extends `${string}.${string}`
  ? never
  : TNumber;

type TupleOf<
  TElement,
  TLength extends number,
  TArray extends readonly unknown[],
  TReadonly extends boolean = false
> = TArray["length"] extends TLength
  ? TArray
  : TReadonly extends true
  ? Readonly<
      TupleOf<
        TElement,
        TLength,
        readonly [TElement, ...Readonly<TArray>],
        TReadonly
      >
    >
  : TupleOf<TElement, TLength, [TElement, ...TArray], TReadonly>;
/**
 * Tuple of specific length.
 *
 * TElement can be a union type.
 *
 * Set TReadonly to true to make it readonly.
 *
 * TLength cannot be a negative number. It can also be a union which will return union of tuples.
 * @example
 * ```
 * const example: Tuple<string, 1 | 2> = ["a"];
 * const example: Tuple<string, 1 | 2> = ["a", "b"];
 * const example: Tuple<string, 1 | 2, true> = ["a", "b"];
 * const example: Tuple<string, 1 | 2> = ["a", "b"] as const; // Results in an error.
 * ```
 */
export type Tuple<
  TElement,
  TLength extends number,
  TReadonly extends boolean = false
> = TLength extends AssertPositive<TLength>
  ? number extends TLength
    ? readonly TElement[]
    : TReadonly extends true
    ? TupleOf<TElement, TLength, readonly [], TReadonly>
    : TupleOf<TElement, TLength, [], TReadonly>
  : never;

/**
 * Returns a tuple where the elements can be repeated and the order of the elements will not matter.
 *
 * The length of the tuple however cannot change.
 * @example
 * ```
 * const example: TupleUnion<["a", 2]> = ["a", 2];
 * const example: TupleUnion<["a", 2]> = [2, "a"];
 * const example: TupleUnion<["a", 2]> = ["a", "a"];
 * const example: TupleUnion<["a", 2]> = [2, 2];
 * const example: TupleUnion<readonly ["a", 2]> = [2, 2] as const;
 * const example: TupleUnion<readonly ["a", 2]> = [2, 2];
 * const example: TupleUnion<["a", 2]> = [2, 2, 2]; // Results in an error.
 * const example: TupleUnion<["a", 2]> = [2]; // Results in an error.
 * const example: TupleUnion<["a", 2]> = [2, 2] as const; // Results in an error since the TTuple is not readonly the resulting tuple cannot be readonly either.
 * ```
 */
export type TupleUnion<TTuple extends AnyImmutableArray> =
  TTuple extends readonly [...[...infer R]]
    ? Tuple<R[IndexOf<R>], TTuple["length"], IsReadOnly<TTuple>>
    : never;

export type Length<T extends readonly unknown[]> = T["length"];

export type TupleOfMinLength<
  T,
  TMin extends number
> = TMin extends AssertPositive<TMin> ? [...Tuple<T, TMin>, ...T[]] : never;

export type TupleOfMaxLength<
  T,
  TMax extends number
> = TMax extends AssertPositive<TMax> ? Partial<[...Tuple<T, TMax>]> : never;
/**
 * Gives tuple of exclusive range.
 *
 * TElement is the type of element in the tuple. It can be a union type.
 *
 * TMax cannot be less than or equal to TMin.
 */
export type TupleOfRangedLength<
  TElement,
  TMin extends number,
  TMax extends number
> = ExclusiveRange<TMax, TMin> extends number
  ? Tuple<TElement, ExclusiveRange<TMax, TMin>>
  : never;

/**
 * Checks if T extends from U. Checks if T is assignable to U.
 */
export type ExtendsFrom<T, U> = T extends U ? true : false;

export type Composite = AnyMutableArray | AnyFunction | AnyObject;

export type ObjectOrArray = AnyMutableArray | AnyObject;
/**
 * Checks if 2 types are equal.
 */
export type Equals<X, Y> = (<T>() => T extends X ? 1 : 2) extends <
  T
>() => T extends Y ? 1 : 2
  ? true
  : false;

type Without<T, U> = { [P in Exclude<keyof T, keyof U>]?: never };
/**
 * Exclusive OR
 */
export type XOR<T, U> = T | U extends object
  ? (T & Without<U, T>) | (U & Without<T, U>)
  : T | U;

export type RecursiveMutable<T> = {
  -readonly [P in keyof T]: RecursiveMutable<T[P]>;
};

export type Mutable<T> = { -readonly [P in keyof T]: T[P] };
/**
 * Recursive {@link Readonly}
 */
export type RecursiveImmutable<T> = {
  +readonly [P in keyof T]: RecursiveImmutable<T[P]>;
};

export type RecursivePartial<T> = {
  [P in keyof T]?: RecursivePartial<T[P]>;
};

export type RecursiveRequired<T> = {
  [P in keyof T]-?: RecursiveRequired<T[P]>;
};

export type LowerOrUpperCase<S extends string> = Lowercase<S> | Uppercase<S>;

export type WithCapitalize<S extends string> = S | Capitalize<S>;
export type WithUncapitalize<S extends string> = S | Uncapitalize<S>;
export type WithLowercase<S extends string> = S | Lowercase<S>;
export type WithUppercase<S extends string> = S | Uppercase<S>;

export type AllCase<S extends string> =
  | Lowercase<S>
  | Uppercase<S>
  | Capitalize<S>
  | Uncapitalize<S>;

export type WithAllCase<S extends string> = S | AllCase<S>;
/**
 * Use with caution. Does not scale well.
 */
export type AnyCase<T extends string> = string extends T
  ? string
  : T extends `${infer F1}${infer F2}${infer R}`
  ? `${LowerOrUpperCase<F1>}${LowerOrUpperCase<F2>}${AnyCase<R>}`
  : T extends `${infer F}${infer R}`
  ? `${LowerOrUpperCase<F>}${AnyCase<R>}`
  : "";
/**
 * Checks to see if something is readonly.
 */
export type IsReadOnly<T> = Readonly<T> extends T ? true : false;
/**
 * Get a union type of the indices of an array.
 */
export type IndexOf<T extends AnyImmutableArray> = ExclusiveRange<T["length"]>;
