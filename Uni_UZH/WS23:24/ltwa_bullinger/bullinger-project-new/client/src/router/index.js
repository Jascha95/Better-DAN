import { createRouter, createWebHistory } from 'vue-router'
import HomeView from "../components/HomeView.vue";
import LetterView from "../components/LetterView.vue";

const router = createRouter({
    history: createWebHistory(import.meta.env.BASE_URL),
    routes: [
        {
            path: '/',
            name: 'home',
            component: HomeView
        },
        {
            path: '/letter-read/:id',
            name: 'letter',
            component: LetterView,
            props: true
        },
    ]
})

export default router
