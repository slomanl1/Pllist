rpushbullet.devices=NA
rpushbullet.key = 'o.rv3O3Q05Vid3kPMlx7uX9Yhd9wSx0EOQ'
rpushbullet.devices[1] = 'Chrome'
rpushbullet.devices[2] = 'E6430LAPTOP'
rpushbullet.devices[3] = 'LarrysIphone'
rpushbullet.names = "Laurence Sloman"
rpushbullet.defaultdevice = 'E6430LAPTOP'
.getKey = function() {
  return(rpushbullet.key)
}
#Require the packages so you can use them
require('RPushbullet')
#pbSetup(rpushbullet.key, '~/.rpushbullet.json', rpushbullet.defaultdevice)
me=pbGetUser(apikey = rpushbullet.key)
summary(me)
pbPost(title='Push2Run E6430LAPTOP',body='run D F')

