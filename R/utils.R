counter = function()
{
  i = 0
  function()
  {
    i <<- i + 1;
    i
  }
  
}

iterator = function(x)
{
  x = force(x)
  i = 0
  function()
  {
    i <<- i + 1;
    x[i]
  }
  
}

nullFnc = function() NULL
emptyListFnc = function() list()
