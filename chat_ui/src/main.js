import Vue from 'vue'
import Vuetify from 'vuetify'
import VueResource from 'vue-resource'
import App from './App'
import router from './router'
import { store } from './store'
import socket from './socket'

import AlertComponent from './components/Shared/Alert.vue'

Vue.use(Vuetify)
Vue.use(VueResource)
Vue.use(socket)

Vue.config.productionTip = false

Vue.component('app-alert', AlertComponent)

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  render: h => h(App)
})
