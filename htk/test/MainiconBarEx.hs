
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.IconBar

main:: IO ()
main =
  do
    main <- initHTk []

    f <- newFrame main []
    pack f [Side AtTop, Fill X]

    ib <- newIconBar f [orient Horizontal]
    pack ib [Side AtLeft]

    img1 <- newImg
    img2 <- openImg
    img3 <- saveImg
    img4 <- printImg
    img5 <- cutImg
    img6 <- copyImg
    img7 <- pasteImg

    b1 <- addButton ib [photo img1]
    b2 <- addButton ib [photo img2]
    b3 <- addButton ib [photo img3]

    addSeparator ib

    b4 <- addButton ib [photo img4]
    b5 <- addButton ib [photo img5]
    b6 <- addButton ib [photo img6]
    b7 <- addButton ib [photo img7]

    ed <- newEditor main [bg "white"]
    pack ed [Side AtBottom, Fill Both, Expand On]

    (htk_destr, _) <- bindSimple main Destroy
    sync (htk_destr)

copyImg = newImage [imgData GIF "R0lGODlhFAAUAMYAAO7u7u7q7hAUEBAQEP///zE0MYuJixgUGDk4Oc3KzYOBg9XO1UpISubm5kpEQTEwMWJdYouFi83GzaylrHN1c//29vbq5sW+vcXGxebi5lpdWlpVUoN5e6SVlIN1c97SzUE8Qc3Ozf/6///y7v/u5vbm3t7OxYOFg1pZWs3CxYN9e2JZWsW2rJyJe3txYoNxatXGtEpESt7e3v/69v/q5vbm1fbi1fbezdW+rFJMUqSlpFpMSnttYtW2pPbq3v/m3vbazfbaxe7SvdWynGJdWt7a3t7KxdXCtN7CtNW6rM2ynNWulM2qi+be5gAAANXW1f///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAAB/mAAIKDhIWGh4iEAQIDjI6NAZGSkwEDBJeYBQYHlJQCmJgIBAkCnZKWCgsKCQQMrJumDZ+gBA4MCA8DDbu8u6gQCRGsEhMUBwPHx7yzoBUVFhcDGJcPBroZlhoREhEbHB0eHwfTBCAEIQMZzCIVIyQkJSYDJ9MOr9gEKCkqKywtLi9gjLvEIAaIBzI+zRgxggaNGjZu4BiQY9UoHSdkWOLQ79+OFjx6DAQlISEBEj5+2LABJIiQIQMmKCCiQGaGIgM+mDByBEmSHkqWMBmJqUmRR0gHOHFyAkUCjApuFplKdeoACRiw0jJateqnpk+b2nxCtmxZS7SKBgIAOw=="]

cutImg = newImage [imgData GIF "R0lGODlhFAAUAIQAAO7u7u7q7gAAAAgECP///5ydnObm5pyZnObi5qShpN7e3jEwMWppat7a3u7m7tXW1QgAAP///////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAABYggII5kWQZoqq5sIAiD0M6v/M5o7co5zwqEQMHwMhiJguMRGDgkkcrnMliQIpPFafOJ6HZrry9V4C17wUyned1NMKtmhXw+By6c9DwdTBAwBHp7aEAGeHMNiDUNL4iLBA5VjYkCkpSNhE6XlpOSjpCWjJydjoUCDy8Pp6aprKxAkKkvEKuttTUhADs="]

newImg = newImage [imgData GIF "R0lGODlhFAAUAMYAAO7q7lJVUkpISkFEQTk4OTEwMSkoKSAgIBgYGBAQEAgICAAAAEpMSubi5ube5t7a3s3Gzb22vZyZnN7e3v////bu9hgUGM3KzUFAQebm5jE0MTEsMfbu7tXOzb2yrIuBeykkKebi3tXKzaSdnP/6///y7u7e1cW2tP/29v/u5v/q3s26rPbq5vbi1dXCtAgMCP/q5v/i1dW+rP/69v/u7v/m3vbezfbaxdW6rAgECPbm3vbazdW6pP/27vbm1e7SvdW2pPbWvfbSvc2ynAAEAO7KrNWunNXW1f/y9u7KtM2qi/bOtO7GrObCpM2li97S1d7Szd7Ozd7Kxd7GvdWynM2qlM2hg////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAAB/6AAIIBAgMEBQYHCAkKCwuCkJAMDQ6UlA8QEQsSj5GCAhMUoqMVDRYXnJ4AGA2jrhUGFKidGbUEoa6jrLKctbUarbmiARgGFo6+GRu4whQcHR4fCxkN1SDBzRQhIiML1dUHzLkkJSUmJ97fCNiuJCglKSor3hP1FuKi5CUsKi0uC/XqvWBHQR+MfjFkAAyogBmJGSVoqKjRwsYNHAAfaMzBDgWKFCl01LCxg8cCjRoBjkLRIwUMHxV3/AByEqU3UR5B6ohBMoiQITU1Egn1ziXMHTd+CCli5OSRp95QIDEaA6kQIUmKKFnwFOqEEjBgtGiB9MeSIkyaOOHadcETKBBRpExx4QIHDypUqjixwjUQADs="]

openImg = newImage [imgData GIF "R0lGODlhFAAUAMYAAO7u7u7q7gAAAObm5ikcECkYEDkoGPauav/Gg//Ce/+6czkkEEEoGO6lav/OnP/WnP/SnP/Ki/+6ezkoEBgQEBgQCBAQCObi5v/Gi//SlP/Cg/+2av+uYv/WpPahWoNVKVI0IEowGFIwGEosGDkgGDEcECAYCP/KlP+2c6xtOVJQUvb29v/6//by9u7m7t7e3t7a3tXSzc3KzcXCxb2+vbS2tLSytPalWlIwINXW1b26vayqrKShpJyVnJSNi4OBg/+yaqxxOVJQSr26tKSlpKSdnIuNi3t9e1I0GNXS1e7m5t7W1cXGxcW+xZSNlHNtaqRtOVpQUube5t7a1c3Kxb22tJyZnCkoKayurNXOzebe3s3GzZSRlGJhYhgUEL15QdXO1Tk4MbSutL22vWppaggEAHNpasW+vayqpJydnDEsMf///////////////////////////////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAAB/6AAIKDhIWFAYiJiQKMjIqPio2SjgKQiI6PkpaVlgGNiAOhoqOjkqMEqAWkq6MGBwgJCgusqwwNDg8QERITFBUVFr7AF8QXCxgPGRobHM3OzwvFDBEdGh4fICEiISMMDAskJQQmjAsRJygpKissKy0BLi8wMTIzNDU2DBgYNzgyAQNcwMhRT4eNHTx6+PhxDEgQISsuAID3guCMITaIFDFyhIECD0iS5FAC6sKSJEyaHCzi5MmCDVCivFhywcUFKVOSUJlRJaOVH1cY3BCBJUuOCy+0nJSxRUcVhFy6eFnwRQhKkTmSgNnpFAuRn2FgMBghpsYMJjLSMpmhYwyWHR1FuJARkKOMGRlibOjQcWavDRtoiKThckRNjhyBAAA7"]

pasteImg = newImage [imgData GIF "R0lGODlhFAAUAOcAAO7u7hgUGEE4GHttEGpZAFpQAO7q7gAEAAAAAAAIACkkCIt5GP///9XOe7SlWjEsEAgECPbu9vby9mJQAP/63v/2zdXOlKydQUpIKc3KzdXO1dXW1ebe5jk0OaSlpKyqrJyZnN7WlNXOg726Yr2uUqSdQYt5EEpEKYOBg4uJg4uNiykoKaylrL26vUE8OTEsKSkoICkkGCkkICAgGBgYGGJhant1e3Ntc4N9gyAgIKShpMXCxZydnFpdWmplalpZWmJlYjEwMbSytLSutJydpJyVnEosEEosGDkkGDEcECAYEBgQCCAcIBgcGJSRlHt9e4uJixAQEObm5qShrJSNlHt5e2ppakE8Qc3Ozebi5pSVlKyurIOFg8XGxUFEQd7e3qSdpIuFi3Nxc0pISkpEQd7a3v/6/4uFg//29vbu7sW6tNXS1UEoGFpVUnt1c6SVlHtxat7SzTEgEP/69v/y7v/q3t7KvcW+vYN9e8W2rJyFc4NxYnttYt7CtM3GzWJdYv/u7v/u5v/m3v/ezdW6pGJhYlJMUhgUEIN5e1JIQaSFe3tpWtW2nHNpalpVWkpMShAMCP/q5v/m1f/i1fbWvf/WvdWulP///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAACP4AAQgcSLCgQYMBBAwgUOAgAAMHEEQ8kEDBAgYNHDyYKBGBAQgGGESQMHICAwoVLFzAkCGDhg0cOiBg4OGDBxAXQ4gYQaKEiRMoUqBQsQJCBBYfWrBw8QJGDBlPZ9CoYeMGjhwIJIDQ8WHHBx4obvTw8cNHjx5AboAIArKFkCFEdBRBYeTIESRJlCxh0uQGE49OdHi4CeLJEQaIEweBEiUHBCktPkwBQaWKlcOJEV9hgCUKgixadPDYsmXuYS4ZuHRh4CUDAygQvoBhoYNKGBRijGRGPIbMlSAIyqgAAeKDDihiTv/IgCKDmQwebEDY8KH6GShhbGBObMYMmjRqEIWseQIFCg8tKKywYdCjOYo2bt7AiQMBC5giUHDgsOFDTuI5aNBBRx112IFAak6oAAUKTwCRBAM/3IFHG3nosQcffUDgBwocViGGFX8owQAaaAASSB2CDDIIIQeiAAUXHBZiyCEMIEKhhYkosggjEHwgxg0/NuLII5AwEEgkkkyiIiWVWBIQADs="]

printImg = newImage [imgData GIF "R0lGODlhFAAUAMYAAO7u7lpZWjEwMWJQSu7q7vbize7ezVpQSjk4MVJIKfbi1f/29v/6/+7izfb29v/2//bu7jk0MVJMKf/y9vbu5u7avebm5lJVUvby7v/y7vbm3ubaze7OrP/u7u7i1e7azTE0MUFAQWJdWvbq5ubKrAgECObi5kpISsW+xayqrFJISubKpPbq3v/u5u7axZSBCEpMSr2+vaSlpHNxc72Zc+7Ci+7Wve7SvQAAAL22Sjk4OaShpGplau7ClO7OtFpQQZSBEN7Se////7SutJydnJyZnLSZau7KrL2ySkpESu7m7vbu9pSVlJyVnJSRlN7We+be5ouJi1pdWs3KzdXS1d7a3t7W3oOFg4uFi3t9e6yurM3GzcXCxc3OzdXW1YOBg4N9g3t5e726vbS2tMXGxdXO1d7e3kE8QUFEQWptarSytL22vWJhYqylrACJAAD/ADk0OWppav///////////////////////////////////////////////////////ywAAAAAFAAUAAAH/oAAgoOEAQIDhIMEi4yNAgUGBwgHjZWNCQoLDJsNB4eWjQEOmwwPDw4QBhGgBBIFpJsTCxQKFRa3uLcXsAwYGRQaGxy5uAkdow8MC78UHh8HuSAgISINIxAZvhQjHgokJSbhJygpKisKGiMsLSwsHgYuBy/hMDEyMyE0NRzdLh8uNm5wwJHDhA4UMnbs4IEjAI0eJADe8HHjB5AgOIQ4GEKkiMeFDY30WHGEBA4kOJIoWSJEyIMhTJo4IQLSYRIcT3BcgGLCggklLTc6cRKlyI4AOEJImXFiCpUqVqxAqWKh5csoUa5gyRLFCQ4nWrZwmdKFrBcvUIIO+QImSogqSDhwhGmSQswYLnjJlKFiRgiDLWfQVBmMJm4aLDvUpFgzZk0MMlyYoNFxtrKXmzjYfGnSpo2bN0N4nIBjuXSIuFKyOCniJM7pQAA7"]

saveImg = newImage [imgData GIF "R0lGODlhFAAUAMYAAO7u7lpdWgAAAO7q7qyurABEQQAwMWJdYouJi7SytABlYghpaghESgA0MaylrABpav///1KurLS2tEGJi97q7lqytObm5pyZnLSutABhYnN1c6yqrFpVWnt5e6SlpObq7lJVUnt9e+bi5oOBg6SdpABdWvby9v/6/0FAQVJQUpSVlEE8QWptakpMSpSRlJyVnHNtc97e3ouNi5SNlIOFg0pISkFEQXNxc0pESlpZWoN9g2ppajk8Od7a3nt1e97W3mplalJMUu7m7tXO1c3Kzb2+vWJhYgAEAMXGxcW+xb22vYuFi9XW1Tk4Of///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAABQAFAAAB/6AAIKDAQKDh4gDiosCBAUGB4uSk4oHCAkKCwoMDZGUkgIOCQ8QEQoPCo+elAISExQQsRAVmZudFrgWARcYGbK/pacauRYCGxsTwL8UHB3EAh4TH8qyICEaItkHIyQlJtQQJygdKdkiAioXKyzgIiAdLeYCLi8XFyAoMMDNGikxMQE0yJDhwsUMg/g0QBCBop+AfwJo0Khh44CMGQgQ0BhxoOGNFA9jCAgxogUAFThy6BhBo0MHDTd2pODR48AOHwdeQDAB4AeQIM1u3GABskcPATBgcBDUQ8iAHjg6tggApIUAoweQ4jDxo8cQIkVydECwwoiRFCuMGs1xxBiSJChKltTQESLI2as9mOjdW8hYjQMwbIAAAnKvYcM5BAgw0iSHkRY89AYCADs="]

{-
upImg = newImage [imgData GIF "R0lGODlhFAAUAKEAAP//////AAAAAP///yH5BAEAAAMALAAAAAAUABQAAAJAnI+py+0Po1Si2iiC
3gLZn21iN4TiWXGdeWqfu7bqW5WyG6RZvbOjyculWkOhTQh6wY7I5I95Q5GSVNChWp0oCgA7"]

refreshImg = newImage [imgData GIF "R0lGODlhFAAUAIQAAPj4+Pz8/Pv7+/b29gYGBvX19ZiYmPr6+oCAgAgICAcHB/Pz8/n5+QUFBYiI
iJaWlv39/f7+/v///wAAAP///////////////////////////////////////////////yH5BAEA
AB8ALAAAAAAUABQAAAU74CeOZGmeaKqu4+Q+rOjOLvuSz6TWccuXOlOC9vvMTgoaiXgiFInF1unY
kwVRDdNtB4XFqNWweEwWhQAAOw=="]

newFolderImg = newImage [imgData GIF "R0lGODlhFAAUAKEAAAAAAP//////AP///yH5BAEAAAMALAAAAAAUABQAAAI5nI+pywjzXlOgzlXl
PRHSbG2AQJYaBGblKkgjC6/WG8dzXd84rO9y5GP1gi0gkTQMhlLMJqcJ3TQKADs="]

deleteFileImg = newImage [imgData GIF "R0lGODlhFAAUAKEAAP////8AAP///////yH5BAEAAAAALAAAAAAUABQAAAIyhI+py+0WUnShTmBp
lVvZi2ShyHSY2WTk84HP6Wrt+8HxaNaLju/rgYIEOZwbcPhKPgoAOw=="]
-}
