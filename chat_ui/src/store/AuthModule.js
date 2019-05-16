import axios from 'axios'
import api from '@/api'

function setAxiosHeaders (token) {
  axios.defaults.headers.common['Authorization'] = 'Bearer ' + token
}

const AuthModule = {
  state: {
    user: null,
    token: null
  },
  mutations: {
    setAuthToken (state, token) {
      state.token = token
      setAxiosHeaders(token)
    },
    setUser (state, payload) {
      state.user = payload
    }
  },
  actions: {
    createUser ({commit, dispatch}, credentials) {
      return new Promise((resolve, reject) => {
        api.createUser(credentials, (response) => {
          resolve(response)
        }, (response) => {
          reject(response)
        })
      })
    },

    login ({commit, dispatch}, credentials) {
      return new Promise((resolve, reject) => {
        api.login(credentials, (response) => {
          commit('setAuthToken', response.data.token)
          dispatch('loadCurrentUserData')
          resolve(response)
        }, (response) => {
          console.log(response)
          dispatch('setError', response)
          reject(response)
        })
      })
    },

    loadCurrentUserData ({commit, dispatch}) {
      api.currentUser((response) => {
        commit('setUser', response.data)
        dispatch('loadUsersList')
      }, (response) => {
        console.log('reject')
        console.log(response)
      })
    }
  },
  getters: {
    currentUser (state) {
      return state.user
    },
    token (state) {
      return state.token
    }
  }
}

export default AuthModule
