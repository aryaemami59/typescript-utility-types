/**
 * Matches any simple object.
 * Does not match arrays or functions.
 */
export type AnyObject = Record<string, unknown>;
/**
 * Does not have access to mutable methods like `push` and `pop`.
 *
 * same as `ReadonlyArray<unknown>`
 * @see {@link ReadonlyArray}
 * @see {@link Array.pop}
 * @see {@link Array.push}
 */
export type AnyImmutableArray = readonly unknown[];
/**
 * has access to mutable methods like `push` and `pop`.
 *
 * extends from {@link AnyImmutableArray}
 *
 * same as `Array<unknown>`
 *
 * @see {@link Array}
 * @see {@link Array.pop}
 * @see {@link Array.push}
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
 * const example: AnyString | "specific string" = "any other string"; // It accepts any other string but also gives "specific string" in intellisense.
 * ```
 */
export type AnyString = string & Record<never, never>;
/**
 * Can be used to generate intellisense for types that are a union of some numbers and any number.
 *
 * works similar to `AnyString`
 *
 * @see {@link AnyString}
 */
export type AnyNumber = number & Record<never, never>;
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
 * same as `FixedLengthTuple<never, 0>` or `FixedLengthTuple<unknown, 0>`
 */
export type EmptyMutableTuple = [];
/**
 * extends from {@link EmptyImmutableArray}
 *
 * same as `FixedLengthTuple<never, 0, true>` or `FixedLengthTuple<unknown, 0, true>`
 */
export type EmptyImmutableTuple = readonly [];
/**
 * Internal helper for `ExclusiveRange`.
 * @see {@link ExclusiveRange}
 */
export type Enumerate<
  TNumber extends number,
  Acc extends number[] = [],
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
  TMin extends number = 0,
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

export type Integer<T extends number> = `${T}` extends `${bigint}` ? T : never;
/**
 * Checks if a number is an integer. TNumber could be a union type but must be a literal number type.
 */
export type AssertInteger<TNumber extends number> = number extends TNumber
  ? TNumber
  : `${TNumber}` extends `${string}.${string}`
  ? never
  : TNumber;
/**
 * Internal Helper for `FixedLengthTuple`.
 * @see {@link FixedLengthTuple}
 */
type TupleOf<
  TElement,
  TLength extends number,
  TArray extends readonly unknown[],
  TReadonly extends boolean = false,
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
 * const foo1: Tuple<string, 1 | 2> = ["a"]; // Pass
 * const foo2: Tuple<string, 1 | 2> = ["a", "b"]; // Pass
 * const foo3: Tuple<string, 1 | 2, true> = ["a", "b"]; // Pass
 *
 * const bar1: Tuple<string, 1 | 2> = ["a", "b"] as const; // Fail
 * ```
 */
export type FixedLengthTuple<
  TElement,
  TLength extends number,
  TReadonly extends boolean = false,
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
 * const foo1: TupleUnion<["a", 2]> = ["a", 2]; // Pass
 * const foo2: TupleUnion<["a", 2]> = [2, "a"]; // Pass
 * const foo3: TupleUnion<["a", 2]> = ["a", "a"]; // Pass
 * const foo4: TupleUnion<["a", 2]> = [2, 2]; // Pass
 * const foo5: TupleUnion<readonly ["a", 2]> = [2, 2] as const; // Pass
 * const foo6: TupleUnion<readonly ["a", 2]> = [2, 2]; // Pass
 *
 * const bar1: TupleUnion<["a", 2]> = [2, 2, 2]; // Fail
 * const bar2: TupleUnion<["a", 2]> = [2]; // Fail
 * const bar3: TupleUnion<["a", 2]> = [2, 2] as const; // Fail since the TTuple is not readonly the resulting tuple cannot be readonly either.
 * ```
 */
export type TupleUnion<TTuple extends AnyImmutableArray> =
  TTuple extends readonly [...[...infer R]]
    ? FixedLengthTuple<R[IndexOf<R>], TTuple["length"], IsReadOnly<TTuple>>
    : never;
/**
 * Get the length of an array as a numeric literal.
 */
export type Length<T extends readonly unknown[]> = T["length"];

export type TupleOfMinLength<
  T,
  TMin extends number,
> = TMin extends AssertPositive<TMin>
  ? [...FixedLengthTuple<T, TMin>, ...T[]]
  : never;

export type TupleOfMaxLength<
  T,
  TMax extends number,
> = TMax extends AssertPositive<TMax>
  ? Partial<[...FixedLengthTuple<T, TMax>]>
  : never;
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
  TMax extends number,
> = ExclusiveRange<TMax, TMin> extends number
  ? FixedLengthTuple<TElement, ExclusiveRange<TMax, TMin>>
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
export type Equal<X, Y> = (<T>() => T extends X ? 1 : 2) extends <
  T,
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
  [Key in keyof T]?: T extends ObjectOrArray
    ? RecursivePartial<T[Key]>
    : T[Key];
};

export type RecursiveRequired<T> = {
  [P in keyof T]-?: RecursiveRequired<T[P]>;
};

export type FirstLetter<S extends string> = S extends `${infer F}${string}`
  ? F
  : never;

export type With<T, U> = T | U;

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
 * __Use with caution. Does not scale well.__
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
 * @see {@link Readonly}
 */
export type IsReadOnly<T> = Readonly<T> extends T ? true : false;
/**
 * Get a union type of the indices of an array.
 */
export type IndexOf<T extends AnyImmutableArray> = Extract<
  keyof T,
  `${number}`
> extends `${infer N extends number}`
  ? N
  : never;
// export type IndexOf<T extends AnyImmutableArray> = ExclusiveRange<T["length"]>;

export type BetterExclude<T, U extends T> = Exclude<T, U>;

export type BetterExtract<T, U extends T> = Extract<T, U>;

export type BetterOmit<T, U extends keyof T> = Omit<T, U>;
/**
 * Useful to flatten the type output to improve type hints shown in editors. And also to transform an interface into a type to aide with assignability.
 */
export type Simplify<T> = {
  [KeyType in keyof T]: T[KeyType];
} & AnyNonNullishValue;
/**
 * Allows creating a union type by combining primitive types and literal types without sacrificing auto-completion in IDEs for the literal type part of the union.
 */
export type LiteralUnion<T, TBase extends number | string> =
  | T
  | (TBase & Record<never, never>);
/**
 * Made for testing purposes.
 */
export type Expect<T extends true> = T;
/**
 * Convert a union type to an intersection type.
 *
 * U must be a union type.
 *
 * If U is a union of only primitives it will result in never. U must include at least one Composite type. U cannot contain more than one primitive type.
 */
export type UnionToIntersection<U> = (
  U extends unknown ? (x: U) => void : never
) extends (x: infer I) => void
  ? I
  : never;
/**
 * Get the values of an object.
 *
 * TObj must be a valid object.
 *
 * You can specify which keys to get the values from.
 */
export type ValuesOf<
  TObj extends AnyObject,
  K extends keyof TObj = keyof TObj,
> = TObj[K];
/**
 * Get Object entries
 * @see {@link Object.entries}
 */
export type ObjectEntries<
  TObj extends AnyObject,
  K extends keyof TObj = keyof TObj,
> = TObj extends { readonly [X in K]: TObj[X] }
  ? ValuesOf<{
      readonly [X in K]: [X, Pick<TObj, X>[X]];
    }>[]
  : never;
// Fetch related
export type RequestMethod = "GET" | "POST" | "PUT" | "DELETE" | "PATCH";

export type HeaderKey =
  | "Content-Type"
  | "Authorization"
  | "User-Agent"
  | "Cache-Control"
  | "Cookie"
  | "Origin"
  | "Referer";

export type AcceptHeaderValue =
  | "application/json"
  | "application/xml"
  | "text/plain"
  | "text/html"
  | "image/jpeg"
  | "image/png"
  | "audio/mp3"
  | "video/mp4"
  | "*/*"
  | "application/*";

export type AddToRequestInit = Partial<{
  method: RequestMethod;
  headers: HeadersInit &
    Record<HeaderKey, string> &
    Record<"Accept", AcceptHeaderValue>;
}>;

export type BetterRequestInit = RequestInit & AddToRequestInit;

export type StringToNumber<T> = T extends `${infer N extends number}`
  ? N
  : never;

export type JoinableItem =
  | string
  | number
  | bigint
  | boolean
  | undefined
  | null;

export type Join<
  T extends readonly JoinableItem[],
  D extends string,
> = T extends []
  ? ""
  : T extends readonly [JoinableItem?]
  ? `${T[0]}`
  : T extends readonly [
      infer F extends JoinableItem,
      ...infer R extends readonly JoinableItem[],
    ]
  ? `${F}${D}${Join<R, D>}`
  : T extends readonly [
      ...infer H extends readonly JoinableItem[],
      infer U extends JoinableItem,
    ]
  ? `${Join<H, D>}${D}${U}`
  : string;
