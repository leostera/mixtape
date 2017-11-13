const R = require('ramda');

/*
 * Tagged Type creation utility.
 *
 * For examples see:
 *    - utils/either.js
 *    - utils/optional.js
 *    - utils/effects.js
 *
 * Note: Most of this code is safety checks for when creating the type and the
 * matchers to ensure the highest reliability possible by failing on bad type
 * definitions as soon as possible.
 */


/******************************************************************************
 *
 * Checks
 *
 ******************************************************************************/

const invalidTypeName = x => !R.is(String, x) || R.isEmpty(x);

const checkNotNil = typeDef => {
  if (R.isNil(typeDef)) {
    throw new TypeError('Type definition cannot be empty');
  }

  return typeDef;
};

const checkValidName = typeDef => {
  if (R.isNil(typeDef.typeName)) {
    throw new TypeError('Type name cannot be undefined');
  }

  if (invalidTypeName(typeDef.typeName)) {
    throw new TypeError(`${typeDef.typeName} is not a valid Type name`);
  }

  return typeDef;
};

const checkValidConstructor = constructor => {
  if (R.isNil(constructor.name) || R.isEmpty(constructor.name)) {
    throw new TypeError('Type constructor name must be provided');
  }

  if (!R.is(Number, constructor.arity)) {
    throw new TypeError('Type constructor arity must be a number');
  }

  if (constructor.arity < 0) {
    throw new TypeError('Type constructor arity must be greater than or equal to 0');
  }
};

const checkValidConstructors = typeDef => {
  if (!R.is(Array, typeDef.constructors) || R.isEmpty(typeDef.constructors)) {
    throw new TypeError('Type constructors must be provided');
  }

  typeDef.constructors.forEach(checkValidConstructor);

  return typeDef;
};

const checkArity = (typeName, constructorName, arity) => (...args) => {
  if (arity !== R.length(args)) {
    throw new TypeError(`Attempting to construct value of type ${typeName} with
constructor ${typeName}.${constructorName} with ${args.length} values [${args}]
but this is an arity ${arity} constructor (takes ${arity} values)`);
  }

  if (arity === 0) return undefined;
  if (arity === 1) return args[0];

  return R.take(arity, args);
};

const checkFunctionTagsNotEmpty = (typeName, typeTags, functionTags) => {
  if (R.isEmpty(functionTags)) {
    throw new TypeError(`
    Attempted to construct matcher for type ${typeName} without any matching.

    Missing cases for:

${typeTags.map(x => `\t-> ${x}`).join('\n')}
    `);
  }
};

const checkMatcherTags = (typeName, typeTags, functionTags) => {
  if (! R.equals(typeTags.sort(), functionTags.sort())) {
    const diff = R.difference(typeTags, functionTags);
    throw new TypeError(`
    Non-exhaustive pattern matching found for ${typeName}.

    Missing cases for:

${diff.map(x => `\t-> ${x}`).join('\n')}
    `);
  }
};


const checkMatcherBranchesNotNil = (typeName, functions) => {
  R.toPairs(functions).forEach(([name, f]) => {
    if (R.isNil(f)) {
      throw new TypeError(`
        Branch ${name} for type ${typeName} is null or undefined :(
        `);
    }
  });
};


/******************************************************************************
 *
 * Helpers
 *
 ******************************************************************************/

const toString = x => {
  if (R.is(Function, x.inspect)) return x.inspect();
  if (R.is(Array, x)) return R.map(toString, x);
  if (R.is(Object, x)) return JSON.stringify(x, null, 2);
  return x;
};

const toStringIfType = x => {
  if (R.isNil(x)) return '';
  if (x.__typeName && x.__constructorName) {
    return `${x.__typeName}.${x.__constructorName}(${toStringIfType(x.value)})`;
  }
  return '';
};

const cleanTypeDefinition = R.compose(
  R.omit(['constructors']),
  R.omit(['typeName'])
);

const createType = typeDef => R.mergeAll([
  cleanTypeDefinition(typeDef),
  ...typeDef.constructors,
  {
    __typeName: typeDef.typeName,
    is: ({__typeName}) => __typeName === typeDef.typeName,
  },
]);


const createConstructor = typeDef => ({name, arity}) => {
  const constructor = {
    [name]: R.compose(
      Object.freeze,
      value => ({

        // Added for redux compatibility
        type: `${typeDef.typeName}.${name}${arity > 0 ? `(${toStringIfType(value)})` : ''}`,
        payload: value,

        __typeName: typeDef.typeName,
        __constructorName: name,

        inspect: () => `${typeDef.typeName}.${name}` + (arity > 0 ? `(${toString(value)})` : ''),

        prototype: typeDef.prototype,

        value,
      }),
      checkArity(typeDef.typeName, name, arity)
    ),
  };

  constructor[name].is = ({__constructorName}) => __constructorName === name;

  return Object.freeze(constructor);
};

const createConstructors = typeDef => R.evolve({
  constructors: R.map(createConstructor(typeDef)),
})(typeDef);

const createMatcher = typeDef => Object.assign({}, typeDef, {
  match: functions => {
    const tags = typeDef.constructors.map(({name}) => name);
    const functionTags = Object.keys(functions);

    checkFunctionTagsNotEmpty(typeDef.typeName, tags, functionTags);
    checkMatcherTags(typeDef.typeName, tags, functionTags);
    checkMatcherBranchesNotNil(typeDef.typeName, functions);

    return ({__typeName, __constructorName, value}) => {
      if (__typeName !== typeDef.typeName) {
        throw new TypeError(`
        Matcher for ${typeDef.typeName} found object of type ${__typeName}.
        `);
      }

      const f = functions[__constructorName];

      if (R.is(Function, f)) return f(value);
      return f;
    };
  },
});

const createSharedPrototype = typeDef => Object.assign({}, typeDef, {
  prototype: { match: typeDef.match },
  __proto__: { match: typeDef.match },
});

// Utility function to create new type objects that provide helper functions
// for:
//    1. Constructing values of this type
//    2. Checking if a value is a value of this type
//    3. Doing case-analysis / exhaustive pattern-matching on these values
//
// type TypeDefinition : {
//    typeName : String             -- the name of the type
//    constructors : [Constructor]  -- a list of constructors for this type
// }
//
// type Constructor : {
//    name : String        -- the name of the method that will create the object
//    arity : Number       -- the amount of values this constructor accepts
// }
//
// type TypeObject : {
//    is : Object -> Bool           -- check if Object is of this type
//    match : Functions -> a -> a   -- creates a safe, exhaustive matcher for
//                                     this specific type considering all
//                                     possible constructors
// }
//
// The idea behind the matcher is to avoid manual type-checking through nested
// if's by specifying the different constructors and how to handle each one of
// them. For a sample type Bool, defined as:
//
// ```
// const Bool = Type({
//  typeName: 'Bool',
//  constructors: [
//    { name: 'True',  arity: 0 },
//    { name: 'False', arity: 0 },
//  ]
// });
//
// const negate = Bool.match({
//  True: Bool.False(),
//  False: Bool.True(),
// });
//
// Bool.true = Bool.True;
//
//
// negate(Bool.False()) // Bool.True()
// negate(1) // throw TypeError("Expecting object of type Bool")
// ```
//
//    Type : TypeDefinition -> TypeObject
const Type = R.compose(
  createType,
  createConstructors,
  createSharedPrototype,
  createMatcher,
  checkValidConstructors,
  checkValidName,
  checkNotNil
);

module.exports = Type;
