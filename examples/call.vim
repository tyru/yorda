echo call(function('has'), ['eval'])

let F = function('has')
echo call(F, ['eval'])

let g:F = function('has')
echo call(g:F, ['eval'])
