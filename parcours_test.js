let graphe1 = [[6,7,8], [1,4], [2], [3,5], [1], [5,7], [], [6,7]]

function parcours(x, g, m, l, o) {
    m[x] = true

    for (let y of g[x - 1]) {
        if (m[y] !== true) {
            parcours(y, g, m, l, o)
        }
    }

    o.h++

    l[o.h] = x
}

let m = {}
let result = {}
let o = { h: 0 }

for (let i = 1; i <= graphe1.length; i++) {
    if (m[i] !== true) {
        parcours(i, graphe1, m, result, o)
    }
}

console.log(result)
