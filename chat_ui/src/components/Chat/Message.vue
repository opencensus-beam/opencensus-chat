<template>
  <div>
    <div class="message" v-for="(message,index) in messages" :class="{own: message.user_id == user.id}">
      <div class="username" v-if="index>0 && messages[index-1].user_id != message.user_id">{{getUserName(message.user_id)}}</div>
      <div class="username" v-if="index == 0">{{getUserName(message.user_id)}}</div>
      <div style="margin-top: 5px"></div>
      <div class="content">
        <div v-html="message.content"></div>
        <chat-image v-if="message.image" :imgsrc="message.image" @imageLoad="imageLoad"></chat-image>
      </div>
    </div>
  </div>
</template>

<script>
  import Image from './Image.vue'

  export default {
    data () {
      return {}
    },
    props: [
      'messages'
    ],
    components: {
      'chat-image': Image
    },
    computed: {
      user () {
        return this.$store.getters.currentUser
      }
    },
    methods: {
      imageLoad () {
        // this.$emit('imageLoad')
      },
      getUser (id) {
        return this.$store.getters.user(id)
      },
      getUserName (id) {
        return this.user.id === id ? 'me' : this.$store.getters.user(id).name
      }
    }
  }
</script>

<style>
  span.emoji {
    font-size: 20px;
    vertical-align: middle;
    line-height: 2;
  }
</style>
