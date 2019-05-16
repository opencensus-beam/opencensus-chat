import axios from 'axios'

axios.defaults.headers.common['Content-Type'] = 'application/json'

export default {
  url: 'http://localhost:4000/api',

  login (credentials, success, reject) {
    axios.post(`${this.url}/session`, credentials).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },

  createUser (credentials, success, reject) {
    axios.post(`${this.url}/users`, {user: credentials}).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },

  currentUser (success, reject) {
    axios.get(`${this.url}/me`).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },

  getUsers (success, reject) {
    axios.get(`${this.url}/users`).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },

  createConversation (params, success, reject) {
    axios.post(`${this.url}/conversations`, {conversation: params}).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },
  getConversations (success, reject) {
    axios.get(`${this.url}/conversations`).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },
  getConversation (id, params, success, reject) {
    axios.get(`${this.url}/conversations/${id}`, { params }).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  },

  sendMessage (params, success, reject) {
    axios.post(`${this.url}/message`, params).then(
      (response) => {
        success(response)
      },
      (response) => {
        reject(response)
      }
    )
  }
}
