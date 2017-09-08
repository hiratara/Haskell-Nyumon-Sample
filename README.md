# Haskell入門 関数型プログラミング言語の基礎と実践 サンプルページ

書籍 [Haskell入門](http://gihyo.jp/book/2017/978-4-7741-9237-6) のサンプルコード集です。

## サンプルのビルドの仕方

### 1章

```
$ stack build chap01-samples
$ stack exec chap01-samples-1-1-4
$ stack exec chap01-samples-1-1-5
```

### 2章

```
$ stack build chap02-samples
$ stack exec chap02-samples-2-3-1-do
$ stack exec chap02-samples-2-3-1-let
$ stack exec chap02-samples-2-3-1-toplevel
$ stack exec chap02-samples-2-3-1-toplevel-2
$ stack exec chap02-samples-2-3-1-where
$ stack exec chap02-samples-2-3-1-where-2
$ stack exec chap02-samples-2-3-1-where-3
$ stack exec chap02-samples-2-4-3
$ stack exec chap02-samples-2-5-io
$ stack exec chap02-samples-2-5-main
$ stack exec chap02-samples-2-5-sample1
$ stack exec chap02-samples-2-5-sample2
$ stack exec chap02-samples-2-5-sample3
$ stack exec chap02-samples-2-6-2-as
$ stack exec chap02-samples-2-6-2-case
$ stack exec chap02-samples-2-6-2-constractor
$ stack exec chap02-samples-2-6-2-irrefutable
$ stack exec chap02-samples-2-6-2-var
$ stack exec chap02-samples-2-6-2-wildcard
$ stack exec chap02-samples-2-6-2-wildcard-2
$ stack exec chap02-samples-2-6-3
$ stack exec chap02-samples-2-6-4-func
$ stack exec chap02-samples-2-6-4-var
$ stack exec chap02-samples-2-7-3
$ stack exec chap02-samples-2-7-4
$ stack exec chap02-samples-2-8-1-fizz-buzz
$ stack exec chap02-samples-2-8-2-fizz-buzz
$ stack exec chap02-samples-2-8-2-fizz-buzz-forM_
$ stack exec chap02-samples-2-9-1-myapp
$ stack exec chap02-samples-2-9-1-myapp-2
```

### 3章

```
$ stack build chap03-samples
$ stack exec chap03-samples-3-2-1
$ stack exec chap03-samples-3-4-3
$ stack exec chap03-samples-3-4-4
$ stack exec chap03-samples-3-5
$ stack exec chap03-samples-3-6
$ stack exec chap03-samples-3-6-data-map
$ stack exec chap03-samples-3-7-1-age
$ stack exec chap03-samples-3-7-1-app-result
$ stack exec chap03-samples-3-7-2
$ stack exec chap03-samples-3-8-3
$ stack exec chap03-samples-3-9
```

### 4章

```
$ stack build chap04-samples
$ stack exec chap04-samples-4-1-3
$ stack exec chap04-samples-4-1-3-do
$ stack exec chap04-samples-4-1-3-do-2
$ stack exec chap04-samples-4-2-1
$ stack exec chap04-samples-4-3-1
$ stack exec chap04-samples-4-3-1-setbuffer
$ stack exec chap04-samples-4-3-3
$ stack exec chap04-samples-4-3-3-counter
$ stack exec chap04-samples-4-3-3-interact
$ stack exec chap04-samples-4-3-3-lazy-io

$ echo "sample text" > sample.txt
$ stack exec chap04-samples-4-3-4

$ echo "ABCDE" > sample
$ stack exec chap04-samples-4-3-5

$ stack exec chap04-samples-4-4-2

$ touch target.txt
$ stack exec chap04-samples-4-4-2-find-file
$ stack exec chap04-samples-4-4-2-find-file-with

$ stack exec chap04-samples-4-5-1
$ stack exec chap04-samples-4-5-1-scoped-type-variables
$ stack exec chap04-samples-4-5-1-finally
$ stack exec chap04-samples-4-5-1-zero
$ stack exec chap04-samples-4-5-1-multiple-exceptions
$ stack exec chap04-samples-4-5-1-catches
$ stack exec chap04-samples-4-5-2
$ stack exec chap04-samples-4-5-3
```

### 5章

```
$ stack build chap05-samples
$ stack exec chap05-samples-5-1-1
$ stack exec chap05-samples-5-1-2
$ stack exec chap05-samples-5-1-3-game
$ stack exec chap05-samples-5-2-1
$ stack exec chap05-samples-5-3-2-human
$ stack exec chap05-samples-5-3-3
$ stack exec chap05-samples-5-4-1-mSafeDiv
$ stack exec chap05-samples-5-4-1-eSafeDiv
$ stack exec chap05-samples-5-4-2-exSafeDiv
$ stack exec chap05-samples-5-5-1-pow
$ stack exec chap05-samples-5-5-1-read-round
$ stack exec chap05-samples-5-6-1
$ stack exec chap05-samples-5-6-2
$ stack exec chap05-samples-5-7
$ stack exec chap05-samples-5-7-1
$ stack exec chap05-samples-5-7-fail
$ stack exec chap05-samples-5-8-2
$ stack exec chap05-samples-5-8-2-count
$ stack exec chap05-samples-5-8-3
$ stack exec chap05-samples-5-8-6
$ stack exec chap05-samples-5-8-6-lb
```

### 7章

```
$ stack build chap07-samples
$ stack exec chap07-samples-7-3-1
$ stack exec chap07-samples-7-4-1
$ stack exec chap07-samples-7-4-2
$ stack exec chap07-samples-7-4-2-column
$ stack exec chap07-samples-7-4-3
$ stack exec chap07-samples-7-5-1
$ stack exec chap07-samples-7-5-2
$ stack exec chap07-samples-7-5-2-monad
$ stack exec chap07-samples-7-5-3
$ stack exec chap07-samples-7-5-4
$ stack exec chap07-samples-7-5-5
$ stack exec chap07-samples-7-6-1
$ stack exec chap07-samples-7-6-1-list-tuple
$ stack exec chap07-samples-7-6-1-complex
$ stack exec chap07-samples-7-6-2
$ stack exec chap07-samples-7-6-3
$ stack exec chap07-samples-7-6-3-manual
$ stack exec chap07-samples-7-6-3-generics
$ stack exec chap07-samples-7-7-1
$ stack exec chap07-samples-7-7-2
$ stack exec chap07-samples-7-8-3
$ stack exec chap07-samples-7-9
$ stack exec chap07-samples-7-10-2
$ stack exec chap07-samples-7-10-3
```

### 8章

```
$ stack build chap08-samples
$ stack exec chap08-samples-8-1-2-single-processor -- 44
$ stack exec chap08-samples-8-1-2-multi-processors -- 44
$ stack exec chap08-samples-8-2-1
$ stack exec chap08-samples-8-2-3
$ stack exec chap08-samples-8-3-1
$ stack exec chap08-samples-8-3-2
$ stack exec chap08-samples-8-4
$ stack exec chap08-samples-8-4-2
$ stack exec chap08-samples-8-4-3
$ stack exec chap08-samples-8-5-1
$ stack exec chap08-samples-8-5-2-async
$ stack exec chap08-samples-8-5-2-withAsync
$ stack exec chap08-samples-8-5-2-race_
$ stack exec chap08-samples-8-5-3
$ stack exec chap08-samples-8-5-3
$ stack exec chap08-samples-8-6-1
$ stack exec chap08-samples-8-6-2
```

### 9章

```
$ stack build --test hjq
$ echo '[ { "age": 25, "name": "佐藤太郎", "tel-number": "111-1111" }, { "age": 26, "name": "斎藤花子", "tel-number": "222-2222" }, { "age": 27, "name": "山田太郎", "tel-number": "333-3333" } ]' | stack exec hjq -- '{"name":.[2].name,"tel-numer":.[2].tel-number}'
```

### 10章

```
$ cat chap10-samples/data/schema.sql | sqlite3 chap10-samples/weight.db
$ stack build --test weight-recorder
$ stack exec weight-recorder -- --db '"chap10-samples/weight.db"' --port 8080
Spock is running on port 8080
```

http://localhost:8080 へアクセス。

### 11章

```
$ stack build auction
$ stack exec auction-server
$ stack exec auction-client
$ stack exec bidder-bot
$ stack exec counter-system
```

