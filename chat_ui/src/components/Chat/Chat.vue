<template>
  <v-layout row>
    <v-flex xs12 sm12 order-xs2 style="position: relative;">
      <div class="chat-container" ref="chatContainer" >
        <!-- <button @click="loadMore">Load More</button> -->
        <message :messages="messages" @imageLoad="scrollToEnd"></message>
      </div>
      <emoji-picker :show="emojiPanel" @close="toggleEmojiPanel" @click="addMessage"></emoji-picker>
      <div class="typer">
        <input type="text" placeholder="Type here..." v-on:keyup.enter="sendMessage" v-model="content">
        <v-btn icon class="blue--text emoji-panel" @click="toggleEmojiPanel">
          <v-icon>mood</v-icon>
        </v-btn>
      </div>
    </v-flex>
    <!-- <v-flex sm2 order-xs1 class="scrollable">
      <chats></chats>
    </v-flex> -->
  </v-layout>
</template>

<script>
  import Message from './Message.vue'
  import EmojiPicker from './EmojiPicker.vue'
  import Chats from './Chats.vue'

  export default {
    data () {
      return {
        content: '',
        chatMessages: [],
        emojiPanel: false,
        currentRef: {},
        loading: false,
        totalChatHeight: 0,

        channel: null
      }
    },
    props: [
      'id'
    ],
    mounted () {
      this.$store.dispatch('loadConversation', this.id)
      this.$socket.joinChat(this.id, {})
    },
    beforeDestroy () {
      this.$socket.leaveChat(this.id)
    },
    watch: {
      id: function (newVal, oldVal) {
        this.$socket.leaveChat(oldVal)

        this.$store.dispatch('loadConversation', newVal)
        this.$socket.joinChat(newVal, {})
      },
      messages: function (newVal, oldVal) {
        this.scrollToEnd()
      }
    },

    components: {
      'message': Message,
      'emoji-picker': EmojiPicker,
      'chats': Chats
    },

    computed: {
      messages () {
        return this.chat ? this.chat.content.map(this.processMessage) : []
      },
      user () {
        return this.$store.getters.currentUser
      },
      chat () {
        return this.$store.getters.chat(this.id)
      }
    },

    methods: {
      processMessage (message0) {
        let message = Object.assign({}, message0) // to avoid data update in the storage and multiple processing
        /*eslint-disable */
        var urlPattern = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig
        /*eslint-enable */
        message.content = message.content
          .replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/"/g, '&quot;')
          .replace(/'/g, '&#039;')
        message.content = message.content.replace(urlPattern, "<a href='$1'>$1</a>")
        /*eslint-disable */
        var imageRegex = /([^\s\']+).(?:jpg|jpeg|gif|png)/i
        /*eslint-enable */
        if (imageRegex.test(message.content)) {
          message.image = imageRegex.exec(message.content)[0]
        }
        var emojiRegex = /([\u{1f300}-\u{1f5ff}\u{1f900}-\u{1f9ff}\u{1f600}-\u{1f64f}\u{1f680}-\u{1f6ff}\u{2600}-\u{26ff}\u{2700}-\u{27bf}\u{1f1e6}-\u{1f1ff}\u{1f191}-\u{1f251}\u{2934}-\u{1f18e}])/gu
        if (emojiRegex.test(message.content)) {
          message.content = message.content.replace(emojiRegex, '<span class="emoji">$1</span>')
        }
        return message
      },
      sendMessage () {
        if (this.content !== '') {
          this.$store.dispatch('sendMessage', { user_id: this.user.id, content: this.content, chatID: this.chat.id })
          this.content = ''
        }
      },
      scrollToEnd () {
        this.$nextTick(() => {
          var container = this.$el.querySelector('.chat-container')
          container.scrollTop = container.scrollHeight
        })
      },
      scrollTo () {
        this.$nextTick(() => {
          let currentHeight = this.$refs.chatContainer.scrollHeight
          let difference = currentHeight - this.totalChatHeight
          var container = this.$el.querySelector('.chat-container')
          container.scrollTop = difference
        })
      },
      addMessage (emoji) {
        this.content += emoji.value
      },
      toggleEmojiPanel () {
        this.emojiPanel = !this.emojiPanel
      },
      loadMore () {
        this.$store.dispatch('loadConversationPart', this.id)
      }
    }
  }
</script>

<style>
  .scrollable {
    overflow-y: auto;
    height: 90vh;
  }
  .typer{
    box-sizing: border-box;
    display: flex;
    align-items: center;
    bottom: 0;
    height: 4.9rem;
    width: 100%;
    background-color: #fff;
    box-shadow: 0 -5px 10px -5px rgba(0,0,0,.2);
  }
  .typer .emoji-panel{
    /*margin-right: 15px;*/
  }
  .typer input[type=text]{
    position: absolute;
    left: 2.5rem;
    padding: 1rem;
    width: 80%;
    background-color: transparent;
    border: none;
    outline: none;
    font-size: 1.25rem;
  }
  .chat-container{
    box-sizing: border-box;
    height: calc(100vh - 9.5rem);
    overflow-y: auto;
    padding: 10px;
    background-color: #f2f2f2;
  }
  .message{
    margin-bottom: 3px;
  }
  .message.own{
    text-align: right;
  }
  .message.own .content{
    background-color: lightskyblue;
  }
  .chat-container .username{
    font-size: 18px;
    font-weight: bold;
  }
  .chat-container .content{
    padding: 8px;
    background-color: lightgreen;
    border-radius: 10px;
    display:inline-block;
    box-shadow: 0 1px 3px 0 rgba(0,0,0,0.2), 0 1px 1px 0 rgba(0,0,0,0.14), 0 2px 1px -1px rgba(0,0,0,0.12);
    max-width: 50%;
    word-wrap: break-word;
    }
  @media (max-width: 480px) {
    .chat-container .content{
      max-width: 60%;
    }
  }

</style>
