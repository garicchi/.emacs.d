# GO CHEAT SHEET

## 可変長配列(スライス)
```go
var list[]int
list = append(list,0)
list = append(list,2)

```

## スライスのループ
```go

//インデックスあり
for i,v := range a{
    
}

//インデックスなし
for _,v := range a{
    
}

```

## map
```go
var m map[string]int
m = make(map[string]int)

m["moge"] = 49

delete(m,"moge")

```

map literals
```go
var m = map[string]int{
		"moge1":1,
		"mage2":2,
	}
	
```

## method
```go

type Vertex struct {
	X, Y float64
}

func (v Vertex) Abs() float64 {
    fmt.Println(v.X)
}

func main() {
	v := Vertex{3, 4}
	v.Abs()
}

```


## エラー実装
```go
type ErrorNegativeSqrt float64

func (e ErrorNegativeSqrt) Error() string{
	return fmt.Sprintf("negative value is not supported %f",e)
}

func Test(x float64) (float64, error) {
	//エラー
	if x<0{
		return 0,ErrorNegativeSqrt(x)
	}
	//正常
	r := x*x
	return r, nil
}

```
