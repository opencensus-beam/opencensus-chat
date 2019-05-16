import {store} from '../store/index'

export default (to, from, next) => {
  if (store.getters.currentUser) {
    next()
  } else {
    next('/login')
  }
}
