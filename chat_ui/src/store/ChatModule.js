import api from '@/api'

const ChatModule = {
  state: {
    chats: [],
    users: []
  },
  mutations: {
    setUsers (state, payload) {
      state.users = payload
    },

    // add or replace
    setUser (state, user) {
      let oldUserState = state.users.find(oldUser => oldUser.id === user.id)

      if (oldUserState) {
        let userIndex = state.users.indexOf(oldUserState)
        state.users.splice(userIndex, 1, user)
      } else {
        state.users.push(user)
      }
    },

    setChats (state, payload) {
      state.chats = payload
    },

    // add or replace
    setChat (state, chat) {
      let oldChat = state.chats.find(oldChat => oldChat.id === chat.id)

      if (oldChat) {
        let chatIndex = state.chats.indexOf(oldChat)
        state.chats.splice(chatIndex, 1, chat)
      } else {
        state.chats.push(chat)
      }
    },

    prependChat (state, chat) {
      let oldChat = state.chats.find(oldChat => oldChat.id === chat.id)

      if (oldChat) {
        chat.content = Array.concat(chat.content, oldChat.content)

        let chatIndex = state.chats.indexOf(oldChat)
        state.chats.splice(chatIndex, 1, chat)
      } else {
        state.chats.push(chat)
      }
    },

    // add new. TODO: make it replaceable to allow editing
    pushMessage (state, {conversationId, content}) {
      state.chats.find(chat => chat.id === conversationId).content.push(content)
    }
  },
  actions: {
    createConversation ({commit}, title) {
      return new Promise((resolve, reject) => {
        api.createConversation({title: title, users: []}, (response) => {
          resolve(response.data.id)
        }, (response) => {
          reject(response)
        })
      })
    },
    loadConversations ({commit}) {
      return new Promise((resolve, reject) => {
        api.getConversations((response) => {
          commit('setChats', response.data)
          resolve(response.data)
        }, (response) => {
          reject(response)
        })
      })
    },
    loadConversation ({commit}, id) {
      return new Promise((resolve, reject) => {
        api.getConversation(id, {}, (response) => {
          commit('setChat', response.data)
          resolve(response.data)
        }, (response) => {
          reject(response)
        })
      })
    },
    loadConversationPart ({commit, getters}, id) {
      return new Promise((resolve, reject) => {
        let conversation = getters.chat(id)
        let lastMessageId

        if (conversation) {
          lastMessageId = conversation.content[0].id
        }

        api.getConversation(id,
          { last_message_id: lastMessageId },
          (response) => {
            commit('prependChat', response.data)
            resolve(response.data)
          }, (response) => {
            reject(response)
          }
        )
      })
    },

    sendMessage ({commit}, payload) {
      const message = {
        conversation_id: payload.chatID,
        user_id: payload.user_id,
        content: payload.content
      }

      api.sendMessage(message, () => {}, () => {})
    },

    loadUsersList ({commit}) {
      api.getUsers((response) => {
        commit('setUsers', response.data)
      }, (response) => {
      })
    },

    // socket callbacks
    newMessage ({commit}, data) {
      commit('pushMessage', data)
    },
    newChat ({commit}, data) {
      commit('setChat', data)
    },
    newUser ({commit}, data) {
      commit('setUser', data)
    }
  },
  getters: {
    users (state) {
      return state.users
    },
    user (state) {
      return (id) => state.users.find(user => user.id === id)
    },
    chats (state) {
      return state.chats
    },
    chat (state) {
      return (id) => state.chats.find(chat => chat.id === id)
    }
  }
}

export default ChatModule
