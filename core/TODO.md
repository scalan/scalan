3. Base loops/tuples/etc. on Shapeless HList to avoid boilerplate. 13.03.2014

4. Prepare open-source infrastructure
    - install Gitlab on dedicated server along with Team City
    - create branching structure for public and internal Scalan-lite repository
    - test Code Migration Workflow (organized around patents/features)
        - Each feature is an independent branch rebased above master in Working repository
        - when patent disclosure submitted
            - corresponding branch is merged in Working master
            - Working is pushed to Internal packaging all commits into single commit in Feature branch of Internal repo
            - Feature branch is merged in Internal master
        - when patent is Filed and paper is accepted (or just decided to open source the feature)
            - commits from Internal are pushed to Public

4. TODO ICFP prepare Internal and Public repositories so that no code occasionally disclosed in history
    - add Option[T] to primitives
    - finish View/UserTypeDef/UserTypeSym structure

4. Implement Iters using Options[T]
    - extend set of primitives (use Haskell streams as inspiration)
    - implement fusion rules directly (without relying on Arrays)
    
5. Implement arity raising rewriting transformation which will normalize types to "true sum-of-products"
    - even after Shapeless will be used, nested tuples may arise during transformations
    - flattening of tuples
    - arity raising of functions (lambdas), methods are not transformed (at least currently)

6. Consider use of LElem: where is it actually needed?

7. Implement FP abstractions
    - State monad
    - Candy state machine from fpinscala
    -