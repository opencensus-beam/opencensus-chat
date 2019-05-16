<template>
  <v-list subheader>
    <v-subheader>Recent Chats</v-subheader>
    <v-list-tile avatar v-for="chat in chats" v-bind:key="chat.title" @click="" :to="/chat/ + chat.id">
      <v-list-tile-content>
        <v-list-tile-title v-html="chat.title"></v-list-tile-title>
      </v-list-tile-content>
      <v-list-tile-action>
        <v-icon>chat_bubble</v-icon>
      </v-list-tile-action>
    </v-list-tile>
  </v-list>
</template>

<script>
  export default{
    data () {
      return {
        recentChats: 'Recent Chats'
      }
    },
    created () {
      this.$store.dispatch('loadConversations')
    },
    computed: {
      chats () {
        return this.$store.getters.chats
      }
    }
  }
</script>
