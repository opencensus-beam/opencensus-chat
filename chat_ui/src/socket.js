import { Socket } from 'phoenix-socket'
import { store } from './store'

// let _store = store()

export default {
  install (Vue) {
    Vue.prototype.$socket = {
      init () {
        this.socket = new Socket('ws://localhost:4000/socket', {params: {token: store.getters['token']}})
        this.chats = {}

        this.socket.connect()

        let globalChannel = this.socket.channel(`chat:all`)

        globalChannel.on('new_chat', payload => {
          store.dispatch('newChat', payload)
        })
        globalChannel.on('new_user', payload => {
          store.dispatch('newUser', payload)
        })
        globalChannel.join()

        return this.socket
      },
      getSocket () {
        return this.socket ? this.socket : this.init()
      },

      joinChat (chatId, params) {
        let socket = this.getSocket()

        let channel = socket.channel(`chat:${chatId}`, params)
        this.chats[chatId] = channel

        channel.on('chat_message', payload => {
          store.dispatch('newMessage', {
            conversationId: payload.conversation_id,
            content: payload
          })
        })

        channel.join()
          .receive('ok', resp => { console.log('Joined successfully', resp) })
          .receive('error', resp => { console.log('Unable to join', resp) })
      },

      leaveChat (chatId) {
        if (this.chats[chatId]) this.chats[chatId].leave()
      }
    }
  }
}
